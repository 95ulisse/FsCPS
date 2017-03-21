module FYANG.Parser

open System
open System.Collections.Generic
open System.Text.RegularExpressions
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open FYANG.Statements
open FYANG.Model

type NamespaceScope = {
    CurrentNamespace: YANGNamespace;
    Prefixes: Dictionary<string, YANGNamespace>
}

/// Context of the parsing operation.
/// Contains a pointer to the current statement and other useful informations.
/// It also implements a stack-like semantics to aid recursion.
type SchemaParserContext(rootStatement: Statement) =

    let _parentStack = Stack<YANGNode>()
    let _namespaceStack = Stack<NamespaceScope>()

    member val RootStatement: Statement = rootStatement
    member val Statement = rootStatement with get, set

    member this.NamespaceScope = _namespaceStack.Peek()

    member this.PushParent n =
        _parentStack.Push(n)

    member this.PopParent() =
        _parentStack.Pop()

    member this.GetParentNode<'T when 'T :> YANGNode>() =
        _parentStack.Peek() :?> 'T

    member this.PushNamespaceScope ns =
        let scope = {
            CurrentNamespace = ns;
            Prefixes = Dictionary<_, _>();
        }
        _namespaceStack.Push scope

    member this.PopNamespaceScope() =
        _namespaceStack.Pop()

    member this.GetNamespace prefix =
        match _namespaceStack.Peek().Prefixes.TryGetValue prefix with
        | (true, ns) -> Some ns
        | _ -> None


/// A parser is a function from a parsing context to a result or an error list.
type SchemaParser<'T> = SchemaParserContext -> Result<'T, SchemaError list>

/// Discriminated union indicating how many times a statement can be repeated.
type Cardinality =
    | Optional
    | Required
    | Many
    | Many1

type StatementSpec<'T> = {
    Name: string;
    Cardinality: Cardinality;
    Parser: SchemaParser<'T>;
}

type StatementSpecHandler<'TNode, 'TArg> = 'TNode -> 'TArg -> SchemaParserContext -> Result<unit, SchemaError list>

/// Helper function to extract a value from a map or return a default if the key is not present
let getOrDefault key def map =
    match Map.tryFind key map with
    | Some(x) -> x
    | None -> def

/// Helper function used to check if the cardinality of a statement is respected
let checkCardinality stmt desc count =
    if count < 0 then
        invalidArg "count" "Invalid negative count"
    match desc.Cardinality with
    | Optional ->
        if count <= 1 then
            None
        else
            Some([ TooManyInstancesOfStatement(stmt) ])
    | Required ->
        if count = 0 then
            Some([ MissingRequiredStatement(stmt, desc.Name) ])
        else if count > 1 then
            Some([ TooManyInstancesOfStatement(stmt) ])
        else
            None
    | Many ->
        None
    | Many1 ->
        if count = 0 then
            Some([ MissingRequiredStatement(stmt, desc.Name) ])
        else
            None

/// Builds a `StatementSpec` for a statement that will be mapped
/// to a property of a YANG node.
let property name (argParser: SchemaParser<'b>) cardinality (handler: StatementSpecHandler<'a, 'b>) : StatementSpec<'a> =

    // Properties can be specified only once at most.
    // Note that this is not a parse-time check, that is why we throw exceptions.
    match cardinality with
    | Optional | Required -> ()
    | _ -> invalidArg "cardinality" "Invalid cardinality for a property. Allowed values are Optional and Required."

    // Constructs the actual parser
    let parser : SchemaParser<_> =
        fun ctx ->
            // Check that the current statement has the expected name
            if ctx.Statement.Name <> name then
                Error([ ExpectedStatement(ctx.Statement, name) ])
            else
                // Parses the argument
                match argParser ctx with
                | Ok(arg) ->
                    // Executes the handler
                    match handler (ctx.GetParentNode()) arg ctx with
                    | Ok(_) -> Ok(ctx.GetParentNode())
                    | Error(l) -> Error(l)
                | Error(l) -> Error(l)

    {
        Name = name;
        Cardinality = cardinality;
        Parser = parser;
    }

/// Builds a `StatementSpec` for a statement that will construct other YANG nodes.
let child name (childSpec: StatementSpec<'b>) cardinality (handler: StatementSpecHandler<'a, 'b>) : StatementSpec<'a> =
    let parser : SchemaParser<_> =
        fun ctx ->
            // Check that the current statement has the expected name
            if ctx.Statement.Name <> name then
                Error([ ExpectedStatement(ctx.Statement, name) ])
            else
                // Parses the child
                match childSpec.Parser ctx with
                | Error(l) -> Error(l)
                | Ok(child) ->
                    let parent = ctx.GetParentNode()
                    match handler parent child ctx with
                    | Ok(_) -> Ok(parent)
                    | Error(l) -> Error(l)

    {
        Name = name;
        Cardinality = cardinality;
        Parser = parser;
    }

/// Creates a spec for a generic YANG node, providing a name, a constructor function
/// and a description of the allowed children.
let createSpec name (ctor: Statement * 'b -> 'a) argumentParser childrenParser : StatementSpec<'a> =
    let parser: SchemaParser<_> =
        fun ctx ->
            // Check that the current statement has the expected name
            if ctx.Statement.Name <> name then
                Error([ ExpectedStatement(ctx.Statement, name) ])
            else
                // Before constructing the node, parses the argument
                let constructedNode =
                    match ctx.Statement.Argument with
                    | None -> Error([ ArgumentExpected(ctx.Statement) ])
                    | Some(arg) ->
                        match argumentParser ctx with
                        | Ok(arg) -> Ok(ctor(ctx.Statement, arg))
                        | Error(l) -> Error(l)

                // Checks the result of the construction
                match constructedNode with
                | Ok(node) ->

                    let currentStatement = ctx.Statement
                    ctx.PushParent(node)

                    let result = childrenParser ctx
                    
                    // Restores the parsing context and returns
                    ctx.PopParent() |> ignore
                    ctx.Statement <- currentStatement
                    result

                | Error(l) -> Error(l)
    
    // The resulting spec
    {
        Name = name;
        Cardinality = Required;
        Parser = parser;
    }



// ------------------------------------------------------------------
// Combinators of parsers for child statements
// ------------------------------------------------------------------

/// Creates a parses that parses any of the given statements in any order.
/// Makes also sure that the cardinalities of the statements are respected.
let anyOf (childrenDesc: StatementSpec<'a> list) : SchemaParser<'a> =
    let childrenMap =
        childrenDesc
        |> List.map (fun desc -> (desc.Name, desc))
        |> Map.ofList
    fun ctx ->
        let parentStatement = ctx.Statement
        let parentNode = ctx.GetParentNode<'a>()

        // Cycles all the child statements and parses them.
        // We also keep track of how many times we encounter each child
        // to be sure that the cardinality is respected.
        let mutable childrenCount = Map.empty
        let mutable result =
            parentStatement.Children |> Seq.fold (fun acc c ->
                // Note that the first error gets propagated,
                // and the parsers for the other children are not called.
                match acc with
                | Ok(_) ->
                    ctx.Statement <- c
                    match Map.tryFind c.Name childrenMap with
                    | Some(childDesc) ->

                        // Checks if parsing the child would exceed the cardinality
                        let childCount = childrenCount |> getOrDefault childDesc.Name 0
                        match checkCardinality c childDesc (childCount + 1) with
                        | Some(l) -> Error(l)
                        | None ->
                            // Executes the parser for the child
                            childrenCount <- childrenCount.Add(childDesc.Name, childCount + 1)
                            match childDesc.Parser ctx with
                            | Ok(_) -> Ok(parentNode)
                            | Error(l) -> Error(l)

                    | None -> Error([ UnexpectedStatement(c) ])
                | Error(_) -> acc
            ) (Ok(parentNode))
        
        // Checks that all the required children are present
        match result with
        | Error(_) -> ()
        | Ok(_) ->
            let requiredChildrenErrors =
                childrenDesc
                |> List.filter (fun c -> c.Cardinality = Required)
                |> List.fold (fun acc c ->
                    match checkCardinality parentStatement c (childrenCount |> getOrDefault c.Name 0) with
                    | Some(l) -> acc @ l
                    | None -> acc
                ) []
            if requiredChildrenErrors <> [] then
                result <- Error(requiredChildrenErrors)

        result

/// Creates a parser that parses a sequence of groups of unordered statements.
/// Makes also sure that the cardinalities of the statements are respected.
let unorderedGroups (childrenSpec: StatementSpec<'a> list list) : SchemaParser<'a> =
    
    // Transforms the description of the children in a list of maps
    let childrenMaps =
        childrenSpec
        |> List.map (fun l ->
            l
            |> List.map (fun desc -> (desc.Name, desc))
            |> Map.ofList
        )

    // Parses a single group of statement specs.
    // Returns `None` if no spec in the group can parse the current statement.
    let parseSingleGroup group (childrenCollection: StatementCollection) index parentStatement (ctx: SchemaParserContext) : int * (SchemaError list) =

        let mutable errors = []

        // Parses all the possible statements.
        // We also keep track of how many times we encounter each child
        // to be sure that the cardinality is respected.
        let mutable i = index
        let mutable stop = false
        let mutable childrenCount = Map.empty
        while (not stop) && i < childrenCollection.Count do
            ctx.Statement <- childrenCollection.[i]
            match Map.tryFind ctx.Statement.Name group with
            | None -> stop <- true
            | Some(childSpec) ->

                // Checks if parsing the child would exceed the cardinality
                let childCount = childrenCount |> getOrDefault childSpec.Name 0
                match checkCardinality ctx.Statement childSpec (childCount + 1) with
                | Some(l) ->
                    errors <- l
                    stop <- true
                | None ->
                    // Executes the parser for the child
                    childrenCount <- childrenCount.Add(childSpec.Name, childCount + 1)
                    match childSpec.Parser ctx with
                    | Ok(_) ->
                        // The statement was successfully parsed: it's time to move on!
                        i <- i + 1
                    | Error(l) ->
                        errors <- l
                        stop <- true
        
        // Checks that all the required children are present
        if List.isEmpty errors then
            let requiredChildrenErrors =
                group
                |> Map.toSeq
                |> Seq.filter (fun (_, spec) -> spec.Cardinality = Required)
                |> Seq.fold (fun acc (_, spec) ->
                    match checkCardinality parentStatement spec (childrenCount |> getOrDefault spec.Name 0) with
                    | Some(l) -> acc @ l
                    | None -> acc
                ) []
            if not (List.isEmpty requiredChildrenErrors) then
                errors <- requiredChildrenErrors

        (i, errors)

    // The parser
    fun ctx ->
        let parentStatement = ctx.Statement
        let parentNode = ctx.GetParentNode<'a>()
        let mutable index = 0

        // The outer `for` loops over the groups,
        // while the `parseSingleGroup` function takes care of 
        let mutable result = Ok(parentNode)
        for group in childrenMaps do
            match result with
            | Error(_) -> () // Stop parsing if we encountered an error
            | Ok(_) ->
                let (newIndex, errors) = parseSingleGroup group parentStatement.Children index parentStatement ctx
                match errors with
                | [] -> index <- newIndex
                | _ -> result <- Error(errors)

        // If there's a child remaining, it means that it couldn't be parsed
        if index < parentStatement.Children.Count then
            result <- Error([ UnexpectedStatement(parentStatement.Children.[index]) ])

        result



// ------------------------------------------------------------------
// Helper functions
// ------------------------------------------------------------------

/// Shortcut for the `property` helper method using Code Quotations instead of plain strings.
/// `expr` is an expression of the following form:
/// <@ fun node -> node.Property @>
/// The accessed property will be converted to snake case and used as statement name.
///
/// The call to `prop cardinality <@ fun x -> x.MyProp @>` is equivalent to
/// `property "my-prop" cardinality (fun x val -> x.MyProp <- val.Value; Ok())`.
let prop argParser cardinality (expr: Expr<'a -> string>) : StatementSpec<'a> =
    
    // First of all, we need to find the property accessed in the quotation
    let propInfo =
        match expr with
        | Lambda(_, PropertyGet(Some(_), p, _)) -> p
        | _ -> invalidArg "expr" "Invalid property expression"

    // Then we normalize the name
    let statementName = Regex.Replace(propInfo.Name, "[A-Z]", "-$1").ToLower().Substring(1)

    property statementName argParser cardinality (fun node value _ ->
        propInfo.SetMethod.Invoke(node, [| box value |]) |> ignore
        Ok()
    )

/// Shortcut for the `child` helper method using Code Quotations instead of plain strings.
/// `expr` is an expression of the following form:
/// <@ fun node -> node.Property @>
/// where Property is a collection. The accessed property will be converted to snake case
/// and used as statement name.
///
/// The call to `chld parser cardinality <@ fun x -> x.MyProp @>` is equivalent to
/// `child "my-prop" parser cardinality (fun x val -> x.MyProp.Add(val); Ok())`.
let chld (childSpec: StatementSpec<'b>) cardinality (expr: Expr<'a -> #ICollection<'b>>) : StatementSpec<'a> =
    
    // First of all, we need to find the property accessed in the quotation
    let propInfo =
        match expr with
        | Lambda(_, PropertyGet(Some(_), p, _)) -> p
        | _ -> invalidArg "expr" "Invalid property expression"

    // Then we normalize the name
    let statementName = Regex.Replace(propInfo.Name, "[A-Z]", "-$1").ToLower().Substring(1)

    child statementName childSpec cardinality (fun node value _ ->
        let coll = propInfo.GetMethod.Invoke(node, null) :?> ICollection<'b>
        coll.Add(value)
        Ok()
    )

/// Argument parser that accepts any string.
let any =
    fun (ctx: SchemaParserContext) ->
        Ok(ctx.Statement.Argument.Value)

/// Helper function that transforms an FParsec parser in an argument parser suitable
/// for a `StatementSpec` construction.
let arg (parser: FParsec.Primitives.Parser<_, unit>) =
    fun (ctx: SchemaParserContext) ->
        match FParsec.CharParsers.run parser ctx.Statement.Argument.Value with
        | FParsec.CharParsers.Success(parsedArg, _, _) -> Ok(parsedArg)
        | FParsec.CharParsers.Failure(e, _, _) -> Error([ ArgumentParserError(ctx.Statement, e) ])

/// Argument parser for an unqualified identifier.
let unqualifiedIdentifier = arg Statements.id

/// Argument parser for a qualified identifier.
/// Parses an identifier and as namespace uses the current namespace of the parsing context.
let identifier =
    fun (ctx: SchemaParserContext) ->
        match unqualifiedIdentifier ctx with
        | Ok(unqualifiedName) ->
            let fullName = {
                Namespace = ctx.NamespaceScope.CurrentNamespace;
                Name = unqualifiedName
            }
            Ok(fullName)
        | Error(l) -> Error(l)

/// Argument parser a Uri.
let uri =
    fun (ctx: SchemaParserContext) ->
        match Uri.TryCreate(ctx.Statement.Argument.Value, UriKind.Absolute) with
        | (true, uri) -> Ok(uri)
        | _ -> Error([ ArgumentParserError(ctx.Statement, "Invalid Uri.") ])

/// Helper operator to execute an action after a parser only if it succeeds.
let (>=>) (spec: StatementSpec<_>) f : StatementSpec<_> =
    { spec with
        Parser = (fun ctx ->
            match spec.Parser ctx with
            | Ok(res) -> f res ctx
            | Error(l) -> Error(l)
        )
    }



// ------------------------------------------------------------------
// Actual parsers
// ------------------------------------------------------------------

let pcontainer : StatementSpec<YANGContainer> =
    createSpec
        "container"
        YANGContainer
        identifier
        (anyOf [])

let pleaf : StatementSpec<YANGLeaf> =
    createSpec
        "leaf"
        YANGLeaf
        identifier
        (anyOf [])

let pleaflist : StatementSpec<YANGLeafList> =
    createSpec
        "leaf-list"
        YANGLeafList
        identifier
        (anyOf [])

let plist : StatementSpec<YANGList> =
    createSpec
        "list"
        YANGList
        identifier
        (anyOf [])

let pmodulerevision : StatementSpec<YANGModuleRevision> =
    createSpec
        "revision"
        YANGModuleRevision
        (arg Statements.dateArg)
        (anyOf [])

let pmodule : StatementSpec<YANGModule> =
    createSpec
        "module"
        YANGModule
        unqualifiedIdentifier
        (unorderedGroups [

            // Module header statements
            [
                property "yang-version" any Optional (fun node value ctx ->
                    if value <> "1" then
                        Error([ UnsupportedYangVersion(ctx.Statement, value) ])
                    else
                        Ok()
                );
                property "namespace" uri Required (fun node value ctx ->
                    node.Namespace <- { Module = node; Uri = value }
                    // Push a new namespace scope if the prefix has also already been parsed
                    if not (isNull node.Prefix) then
                        ctx.PushNamespaceScope(node.Namespace)
                        ctx.NamespaceScope.Prefixes.Add(node.Prefix, node.Namespace)
                    Ok()
                );
                property "prefix" unqualifiedIdentifier Required (fun node value ctx ->
                    node.Prefix <- value
                    // Push a new namespace scope if the namespace has also already been parsed
                    if node.Namespace <> YANGNamespace.Empty then
                        ctx.PushNamespaceScope(node.Namespace)
                        ctx.NamespaceScope.Prefixes.Add(node.Prefix, node.Namespace)
                    Ok()
                )
            ];

            // Linkage statements

            // Meta statements
            [
                prop any Optional <@ fun x -> x.Contact @>;
                prop any Optional <@ fun x -> x.Organization @>;
                prop any Optional <@ fun x -> x.Description @>;
                prop any Optional <@ fun x -> x.Reference @>;
            ];

            // Revision statements
            [
                chld pmodulerevision Many <@ fun x -> x.Revisions @>;
            ];

            // Body statements
            [
                chld pcontainer Many <@ fun x -> x.Containers @>;
                chld pleaf      Many <@ fun x -> x.Leafs @>;
                chld pleaflist  Many <@ fun x -> x.LeafLists @>;
                chld plist      Many <@ fun x -> x.Lists @>;
            ]
        ])

    // When the module has been completely parsed,
    // pop the namespace scope from the stack
    >=> (fun m ctx ->
        ctx.PopNamespaceScope() |> ignore
        Ok(m)
    )