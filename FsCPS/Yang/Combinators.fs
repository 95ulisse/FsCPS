module internal FsCPS.Yang.Combinators

open System.Collections
open System.Collections.Generic
open System.Text.RegularExpressions
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open FsCPS


/// Discriminated union indicating how many times a statement can be repeated.
type Cardinality =
    | Optional
    | Required
    | Many
    | Many1


/// Specification of a single YANG statements.
type StatementSpec<'T> = {
    Name: string;
    Cardinality: Cardinality;
    Parser: SchemaParser<'T>;
}


/// Callback used by the various parsers to handle the parsing of a statement.
/// It is usually called after a statement has been parsed, and this function
/// takes care of deciding what to do with the new value.
type SchemaParserCallback<'TNode, 'TArg> = 'TNode -> 'TArg -> SchemaParserContext -> SchemaParserResult<unit>


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
let property name (argParser: SchemaParser<'b>) cardinality (handler: SchemaParserCallback<'a, 'b>) : StatementSpec<'a> =

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
                    match handler (ctx.Scope.ParentNode :?> 'a) arg ctx with
                    | Ok(_) -> Ok(ctx.Scope.ParentNode :?> 'a)
                    | Error(l) -> Error(l)
                | Error(l) -> Error(l)

    {
        Name = name;
        Cardinality = cardinality;
        Parser = parser;
    }

/// Builds a `StatementSpec` for a statement that will construct other YANG nodes.
let child (childSpec: StatementSpec<'b>) cardinality (handler: SchemaParserCallback<'a, 'b>) : StatementSpec<'a> =
    let parser : SchemaParser<_> =
        fun ctx ->
            // Check that the current statement has the expected name
            if ctx.Statement.Name <> childSpec.Name then
                Error([ ExpectedStatement(ctx.Statement, childSpec.Name) ])
            else
                // Parses the child
                match childSpec.Parser ctx with
                | Error(l) -> Error(l)
                | Ok(child) ->
                    let parent = ctx.Scope.ParentNode :?> 'a
                    match handler parent child ctx with
                    | Ok(_) -> Ok(parent)
                    | Error(l) -> Error(l)

    {
        Name = childSpec.Name;
        Cardinality = cardinality;
        Parser = parser;
    }

/// Creates a spec for a generic YANG node, providing a name, a constructor function
/// and a description of the allowed children.
let createSpec<'a, 'b when 'a :> YANGNode> name (ctor: 'b -> 'a) argumentParser childrenParser : StatementSpec<'a> =
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
                        | Ok(arg) -> Ok(ctor(arg))
                        | Error(l) -> Error(l)

                // Checks the result of the construction
                match constructedNode with
                | Ok(node) ->

                    // Save the original statement in the node
                    node.OriginalStatement <- Some(ctx.Statement)

                    // We are entering a new block, so we push a new scope to the context
                    ctx.PushScope(ctx.Statement, node)

                    // Parses the children
                    let result = childrenParser ctx
                    
                    // Restores the parsing context and returns
                    ctx.PopScope() |> ignore
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
        let parentNode = ctx.Scope.ParentNode :?> 'a

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

                    | None ->
                        // Let the options decide what to do with the unknown statement
                        ctx.Options.UnknownStatement c
                        |>> (fun _ -> parentNode)
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
    
    // Helper function used to see if a statement is completely unknown
    // or if it belongs to another group of statements
    let isUnknownStatement =
        let names = childrenSpec |> List.collect (List.map (fun x -> x.Name))
        fun x ->
            not (List.contains x names)

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
            | None ->

                // Stop parsing only if we got a statement of another group.
                // If we got a completely unknown statement, let the handler decide.
                if isUnknownStatement ctx.Statement.Name then
                    match ctx.Options.UnknownStatement ctx.Statement with
                    | Ok () ->
                        i <- i + 1 // Consume the statement
                    | Error l ->
                        errors <- l
                        stop <- true
                else
                    stop <- true

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
        let parentNode = ctx.Scope.ParentNode :?> 'a
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
        match result with
        | Error(_) -> ()
        | Ok(_) ->
            if index < parentStatement.Children.Count then
                result <- ctx.Options.UnknownStatement parentStatement.Children.[index] |>> (fun _ -> parentNode)

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
let prop (argParser: SchemaParser<'b>) cardinality (expr: Expr<'a -> 'b>) : StatementSpec<'a> =
    
    // First of all, we need to find the property accessed in the quotation
    let propInfo =
        match expr with
        | Lambda(_, PropertyGet(Some(_), p, _)) -> p
        | _ -> invalidArg "expr" "Invalid property expression"

    // Then we normalize the name
    let statementName = Regex.Replace(propInfo.Name, "[A-Z]", "-$&").ToLower().Substring(1)

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
let chld (childSpec: StatementSpec<_>) cardinality (expr: Expr<'a -> #IList>) : StatementSpec<'a> =
    
    // First of all, we need to find the property accessed in the quotation
    let propInfo =
        match expr with
        | Lambda(_, PropertyGet(Some(_), p, _)) -> p
        | _ -> invalidArg "expr" "Invalid property expression"

    child childSpec cardinality (fun node value _ ->
        let coll = propInfo.GetMethod.Invoke(node, null) :?> IList
        coll.Add(value) |> ignore
        Ok()
    )

/// Creates a StatementSpec whose SchemaParser forwards to the reference cell.
let createSpecForwaredToRef name cardinality : StatementSpec<'a> * StatementSpec<'a> ref =
    let cell =
        FSharp.Core.Operators.ref(
            {
                Name = name;
                Cardinality = cardinality;
                Parser = fun ctx -> Ok(Unchecked.defaultof<'a>)
            }
        )
    let spec = {
        Name = name;
        Cardinality = cardinality;
        Parser =
            fun ctx ->
                (!cell).Parser ctx
    }
    (spec, cell)