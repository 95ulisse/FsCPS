module FsCPS.Yang.Parser

open System
open System.Collections
open System.Collections.Generic
open System.Text.RegularExpressions
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open FsCPS.Yang.Statements
open FsCPS.Yang.Model

/// Result of a parser.
/// It can either be an error or a success, in which case the parsed element is returned.
type SchemaParserResult<'T> = Result<'T, SchemaError list>


/// Scope of a single statement.
/// A scope lives only between the children of a statement,
/// or in other words, a scope begins with a "{" and ends with a "}".
type Scope = {
    Module: YANGModule;
    ParentStatement: Statement;
    ParentNode: YANGNode;
    Prefixes: Dictionary<string, YANGModule>;
    Types: Dictionary<YANGName, YANGType>;
}


/// Context of the parsing operation.
/// Contains a pointer to the current statement and other useful informations.
/// It also implements a stack-like semantics for scopes to aid recursion.
type SchemaParserContext(rootStatement: Statement) =

    let _scopeStack = Stack<Scope>()

    /// Root statement from which the parsing started.
    member val RootStatement: Statement = rootStatement

    /// Current statement that is being parsed.
    member val Statement = rootStatement with get, set

    /// Current scope.
    member this.Scope
        with get() = _scopeStack.Peek()

    /// Pushes a new scope with the given parent statements and node to the stack.
    member this.PushScope(parentStatement, parentNode: YANGNode) =
        if parentNode :? YANGModule then
            _scopeStack.Push({
                Module = parentNode :?> YANGModule;
                ParentStatement = parentStatement;
                ParentNode = parentNode;
                Prefixes = Dictionary<_, _>();
                Types = Dictionary<_, _>();
            })
        else
            _scopeStack.Push({
                this.Scope with
                    ParentStatement = parentStatement;
                    ParentNode = parentNode;
                    Prefixes = Dictionary<_, _>();
                    Types = Dictionary<_, _>();
            })

    /// Pops the last scope from the stack.
    member __.PopScope() =
        _scopeStack.Pop()

    /// Returns the type with the given name.
    member this.GetType(name: YANGName) =
        _scopeStack
        |> Seq.tryPick (fun scope ->
            match scope.Types.TryGetValue(name) with
            | (true, t) -> Some(t)
            | _ -> None
        )

    /// Registers a new type in the current scope.
    /// Returns an error if the new type would shadow an already defined one.
    member this.RegisterType(t: YANGType) =
        match YANGPrimitiveTypes.FromName(t.Name.Name) with
        | Some(shadowedType) -> Error([ ShadowedType(t.OriginalStatement.Value, shadowedType) ])
        | None ->
            match this.GetType(t.Name) with
            | Some(shadowedType) ->
                Error([ ShadowedType(t.OriginalStatement.Value, shadowedType) ])
            | None ->
                this.Scope.Types.Add(t.Name, t)
                Ok()

    /// Resolves a reference to a type.
    member this.ResolveTypeRef(prefix: string option, name: string) =
        match prefix, YANGPrimitiveTypes.FromName(name) with
        
        // Reference to a primitive type
        | None, Some(primitiveType) ->
            Some(primitiveType)

        // We can either have an unprefixed reference, or a prefixed reference that is using
        // the same prefix of the module we are parsing. In both cases, resolve from the current scope.
        | None, None ->
            this.GetType({ Namespace = this.Scope.Module.Namespace; Name = name })
        | Some(p), _ when p = this.Scope.Module.Prefix ->
            this.GetType({ Namespace = this.Scope.Module.Namespace; Name = name })

        // We have a prefixed reference, so resolve the module associated to the prefix,
        // then search between it's exported types
        | Some(p), _ ->
            match this.Scope.Prefixes.TryGetValue(p) with
            | (true, m) ->
                let fullName = { Namespace = m.Namespace; Name = name }
                match m.ExportedTypes.TryGetValue(fullName) with
                | (true, t) -> Some(t)
                | _ -> None
            | _ -> None


/// A parser is a function from a parsing context to a result or an error list.
type SchemaParser<'T> = SchemaParserContext -> SchemaParserResult<'T>


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
    let cell = FSharp.Core.Operators.ref({
        Name = name;
        Cardinality = cardinality;
        Parser = fun ctx -> Ok(Unchecked.defaultof<'a>)
    })
    let spec = {
        Name = name;
        Cardinality = cardinality;
        Parser =
            fun ctx ->
                (!cell).Parser ctx
    }
    (spec, cell)

/// Argument parser that accepts any string.
let any : SchemaParser<string> =
    fun ctx ->
        Ok(ctx.Statement.Argument.Value)

/// Helper function that transforms an FParsec parser in an argument parser suitable
/// for a `StatementSpec` construction.
let arg (parser: FParsec.Primitives.Parser<_, unit>) : SchemaParser<_> =
    fun ctx ->
        match FParsec.CharParsers.run parser ctx.Statement.Argument.Value with
        | FParsec.CharParsers.Success(parsedArg, _, _) -> Ok(parsedArg)
        | FParsec.CharParsers.Failure(e, _, _) -> Error([ ArgumentParserError(ctx.Statement, e) ])

/// Argument parser for an unqualified identifier.
let unqualifiedIdentifier = arg Statements.id

/// Argument parser for an unsigned int.
let uint = arg FParsec.CharParsers.puint32

/// Argument parser for a positive integer.
let positiveInt =
    fun ctx ->
        arg FParsec.CharParsers.pint32 ctx
        |> Result.bind (fun res ->
            if res < 0 then
                Error([ ArgumentParserError(ctx.Statement, "Expected non negative integer.") ])
            else
                Ok(res)
        )

/// Argument parser for a positive integer or the string "unbounded",
/// which gets parsed to Int32.MaxValue.
let positiveIntOrUnbounded =
    let rawParser =
        FParsec.Primitives.choice [
            FParsec.CharParsers.pint32;
            FParsec.Primitives.(|>>) (FParsec.CharParsers.pstring "unbounded") (fun _ -> Int32.MaxValue)
        ]
    fun ctx ->
        arg rawParser ctx
        |> Result.bind (fun res ->
            if res < 0 then
                Error([ ArgumentParserError(ctx.Statement, "Expected non negative integer.") ])
            else
                Ok(res)
        )

/// Argument parser for a boolean.
let boolean : SchemaParser<bool> =
    fun ctx ->
        match Boolean.TryParse(ctx.Statement.Argument.Value) with
        | (true, b) -> Ok(b)
        | _ -> Error([ ArgumentParserError(ctx.Statement, "Invalid boolean value.") ])

/// Argument parser for a qualified identifier.
/// Parses an identifier and as namespace uses the current namespace of the parsing context.
let identifier : SchemaParser<YANGName> =
    fun ctx ->
        match unqualifiedIdentifier ctx with
        | Ok(unqualifiedName) ->
            let fullName = {
                Namespace = ctx.Scope.Module.Namespace;
                Name = unqualifiedName
            }
            Ok(fullName)
        | Error(l) -> Error(l)

/// Argument parser for a reference to a type.
let typeRef : SchemaParser<YANGType> =
    fun ctx ->
        match arg Statements.ref ctx with
        | Error(l) -> Error(l)
        | Ok((prefix, name)) -> 
            match ctx.ResolveTypeRef(prefix, name) with
            | Some(t) -> Ok(t)
            | None -> Error([ UnresolvedTypeRef(ctx.Statement, ctx.Statement.Argument.Value) ])

/// Argument parser for a Uri.
let uri : SchemaParser<Uri> =
    fun ctx ->
        match Uri.TryCreate(ctx.Statement.Argument.Value, UriKind.Absolute) with
        | (true, uri) -> Ok(uri)
        | _ -> Error([ ArgumentParserError(ctx.Statement, "Invalid Uri.") ])

/// Argument parser for a Regex.
let regex : SchemaParser<Regex> =
    fun ctx ->
        try
            Ok(Regex(ctx.Statement.Argument.Value, RegexOptions.Compiled))
        with
        | _ -> Error([ ArgumentParserError(ctx.Statement, "Invalid Regex.") ])

/// Argument parser for a status statement.
let status : SchemaParser<YANGStatus> =
    fun ctx ->
        match ctx.Statement.Argument.Value with
        | "current" -> Ok(YANGStatus.Current)
        | "deprecated" -> Ok(YANGStatus.Deprecated)
        | "obsolete" -> Ok(YANGStatus.Obsolete)
        | _ -> Error([ ArgumentParserError(ctx.Statement, "Invalid status. The passible values are `current`, `deprecated` or `obsolete`.") ])

/// Argument parser for a ordered-by statement.
let orderedBy : SchemaParser<YANGListOrderedBy> =
    fun ctx ->
        match ctx.Statement.Argument.Value with
        | "system" -> Ok(YANGListOrderedBy.System)
        | "user" -> Ok(YANGListOrderedBy.User)
        | _ -> Error([ ArgumentParserError(ctx.Statement, "Invalid ordering. The passible values are `system` or `user`.") ])

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

type YANGTypeRef(t: YANGType) =
    inherit YANGNode()
    member val Value = t with get, set
    member val AdditionalRestrictions = ResizeArray<YANGTypeRestriction>()
    member val AdditionalProperties = Dictionary<string, obj>()

/// Shorthand function to create a spec for a statement representing a type restriction.
/// Automatically configures the optional substatements `description`, `error-app-tag`,
/// `error-message` and `reference`.
let createRestrictionSpec name (ctor: 'a -> #YANGTypeRestriction) (argParser: SchemaParser<'a>) =
    createSpec name ctor argParser (anyOf [
        prop any Optional <@ fun x -> x.Description @>;
        prop any Optional <@ fun x -> x.ErrorAppTag @>;
        prop any Optional <@ fun x -> x.ErrorMessage @>;
        prop any Optional <@ fun x -> x.Reference @>;
    ])

    // Before returning the restriction, check that the enclosing type supports it
    >=> (fun r ctx ->
        let t = ctx.Scope.ParentNode :?> YANGTypeRef
        if t.Value.CanBeRestrictedWith(r) then
            Ok(r)
        else
            Error([ InvalidTypeRestriction(r.OriginalStatement.Value, t.Value) ])
    )

/// Checks if an enum value is valid: checks the uniqueness of the name
/// and of the value. If the value has not been provided, it automatically computes
/// and assignes the next one.
let checkNewEnumValue (node: YANGTypeRef) (enumValue: YANGEnumValue) =
    let currentValues =
        match node.AdditionalProperties.TryGetValue("enum-values") with
        | (true, l) -> l :?> seq<YANGEnumValue>
        | _ -> Seq.empty
    
    let mutable computedValue = None
    let mutable result = Ok()
    Seq.append currentValues (YANGPrimitiveTypes.AllEnumValues node.Value)
    |> Seq.forall (fun v ->
        if v.Name = enumValue.Name then
            result <- Error([ DuplicateEnumName(enumValue) ])
            false
        else if enumValue.Value.IsSome && enumValue.Value.Value = v.Value.Value then
            result <- Error([ DuplicateEnumValue(enumValue) ])
            false
        else
            computedValue <-
                match computedValue with
                | Some(c) -> Some(max c v.Value.Value)
                | None -> Some(v.Value.Value)
            true
    ) |> ignore

    match enumValue.Value, computedValue with
    | Some _, _ -> ()
    | None, Some c -> enumValue.Value <- Some(c + 1)
    | None, None -> enumValue.Value <- Some(0)

    result



let prangeRestriction : StatementSpec<YANGRangeRestriction> =
    createRestrictionSpec
        "range"
        YANGRangeRestriction
        (arg Statements.rangeDescription)

let plengthRestriction : StatementSpec<YANGLengthRestriction> =
    createRestrictionSpec
        "length"
        YANGLengthRestriction
        (arg Statements.lengthRangeDescription)

let ppatternRestriction : StatementSpec<YANGPatternRestriction> =
    createRestrictionSpec
        "pattern"
        YANGPatternRestriction
        regex

let penum : StatementSpec<YANGEnumValue> =
    createSpec
        "enum"
        YANGEnumValue
        unqualifiedIdentifier
        (anyOf [
            prop any                              Optional <@ fun x -> x.Description @>;
            prop any                              Optional <@ fun x -> x.Reference @>;
            prop status                           Optional <@ fun x -> x.Status @>;
            property "value" (arg FParsec.CharParsers.pint32) Optional (fun node value _ ->
                node.Value <- Some value
                Ok()
            );
        ])

let ptype : StatementSpec<YANGTypeRef> =
    createSpec
        "type"
        YANGTypeRef
        typeRef
        (anyOf [
            property "fraction-digits" uint Optional (fun node digits _ ->
                node.AdditionalProperties.Add("fraction-digits", int digits)
                Ok()
            );

            child penum Many (fun node enumValue _ ->
                match checkNewEnumValue node enumValue with
                | Ok() ->
                    match node.AdditionalProperties.TryGetValue("enum-values") with
                    | (true, l) -> (l :?> IList<YANGEnumValue>).Add(enumValue)
                    | _ -> node.AdditionalProperties.Add("enum-values", ResizeArray([ enumValue ]))
                    Ok()
                | Error(l) -> Error(l)
            );

            chld prangeRestriction   Optional <@ fun x -> x.AdditionalRestrictions @>;
            chld plengthRestriction  Optional <@ fun x -> x.AdditionalRestrictions @>;
            chld ppatternRestriction Many     <@ fun x -> x.AdditionalRestrictions @>;
        ])

let ptypedef : StatementSpec<YANGType> =
    createSpec
        "typedef"
        YANGType
        identifier
        (anyOf [
            child ptype Required (fun node value _ ->
                node.BaseType <- Some value.Value
                node.Restrictions.AddRange(value.AdditionalRestrictions)
                value.AdditionalProperties |> Seq.iter (fun pair -> node.SetProperty(pair.Key, pair.Value))
                
                // Check that the required properties have been provided
                node.CheckRequiredProperties()
            );

            property "default" any    Optional (fun x value _ -> x.Default <- value; Ok());
            prop               any    Optional <@ fun x -> x.Description @>;
            prop               any    Optional <@ fun x -> x.Reference @>;
            prop               status Optional <@ fun x -> x.Status @>;
            prop               any    Optional <@ fun x -> x.Units @>;
        ])

    // If a default value has been provided, check that it's valid
    >=> (fun t ctx ->
        if not (isNull t.Default) then
            match t.IsValid(t.Default) with
            | Ok _ -> Ok(t)
            | Error(restriction) -> Error([ InvalidDefault(t, restriction) ])
        else
            Ok(t)
    )

    // Registers the new type in the context automatically
    >=> (fun t ctx ->
        ctx.RegisterType(t)
        |> Result.map (fun _ -> t)
    )


// Parsers for all the data nodes.
// They are all forwarded because they are all mutually recursive.
let pcontainer, pcontainerRef: StatementSpec<YANGContainer> * _  = createSpecForwaredToRef "container"  Required
let pleaf,      pleafRef:      StatementSpec<YANGLeaf> * _       = createSpecForwaredToRef "leaf"       Required
let pleaflist,  pleaflistRef:  StatementSpec<YANGLeafList> * _   = createSpecForwaredToRef "leaf-list"  Required
let plist,      plistRef:      StatementSpec<YANGList> * _       = createSpecForwaredToRef "list"       Required

pcontainerRef :=
    createSpec
        "container"
        YANGContainer
        identifier
        (anyOf [
            // Container-specific properties
            prop any    Optional <@ fun x -> x.Presence @>;

            // Common properties for data nodes
            prop any    Optional <@ fun x -> x.Description @>;
            prop any    Optional <@ fun x -> x.Reference @>;
            prop status Optional <@ fun x -> x.Status @>;

            // Typedefs
            child ptypedef Many (fun _ _ _ -> Ok());

            // Data statements
            chld pcontainer Many <@ fun x -> x.DataNodes @>;
            chld pleaf      Many <@ fun x -> x.DataNodes @>;
            chld pleaflist  Many <@ fun x -> x.DataNodes @>;
            chld plist      Many <@ fun x -> x.DataNodes @>;
        ])

pleafRef :=
    createSpec
        "leaf"
        YANGLeaf
        identifier
        (anyOf [
            // Leaf-specific properties
            property "default" any     Optional (fun x value _ -> x.Default <- value; Ok());
            prop               any     Optional <@ fun x -> x.Units @>;
            prop               boolean Optional <@ fun x -> x.Mandatory @>;
            child ptype Required (fun node value _ ->
                let newType = YANGType(value.Value.Name, BaseType = Some value.Value)
                newType.Restrictions.AddRange(value.AdditionalRestrictions)
                value.AdditionalProperties |> Seq.iter (fun pair -> newType.SetProperty(pair.Key, pair.Value))
                node.Type <- newType
                
                // Check that the required properties have been provided
                newType.CheckRequiredProperties()
            );

            // Common properties for data nodes
            prop any     Optional <@ fun x -> x.Description @>;
            prop any     Optional <@ fun x -> x.Reference @>;
            prop status  Optional <@ fun x -> x.Status @>;
        ])

    // If a default value has been provided, check that it's valid
    >=> (fun leaf ctx ->
        if not (isNull leaf.Default) then
            match leaf.Type.IsValid(leaf.Default) with
            | Ok _ -> Ok(leaf)
            | Error(restriction) -> Error([ InvalidLeafDefault(leaf, restriction) ])
        else
            Ok(leaf)
    )

    // Mandatory leafs cannot have a default value
    >=> (fun leaf ctx ->
        if leaf.Mandatory && not (isNull leaf.Default) then
            Error([ SchemaError(leaf, "Mandatory leafs cannot have a default value.") ])
        else
            Ok(leaf)
    )

pleaflistRef :=
    createSpec
        "leaf-list"
        YANGLeafList
        identifier
        (anyOf [
            // LeafList-specific properties
            prop positiveInt            Optional <@ fun x -> x.MinElements @>
            prop positiveIntOrUnbounded Optional <@ fun x -> x.MaxElements @>
            prop orderedBy              Optional <@ fun x -> x.OrderedBy @>;
            prop any                    Optional <@ fun x -> x.Units @>;
            child ptype Required (fun node value _ ->
                let newType = YANGType(value.Value.Name, BaseType = Some value.Value)
                newType.Restrictions.AddRange(value.AdditionalRestrictions)
                value.AdditionalProperties |> Seq.iter (fun pair -> newType.SetProperty(pair.Key, pair.Value))
                node.Type <- newType
                
                // Check that the required properties have been provided
                newType.CheckRequiredProperties()
            );

            // Common properties for data nodes
            prop any     Optional <@ fun x -> x.Description @>;
            prop any     Optional <@ fun x -> x.Reference @>;
            prop status  Optional <@ fun x -> x.Status @>;
        ])

plistRef :=
    createSpec
        "list"
        YANGList
        identifier
        (anyOf [
            // List-specific properties
            prop positiveInt            Optional <@ fun x -> x.MinElements @>
            prop positiveIntOrUnbounded Optional <@ fun x -> x.MaxElements @>
            prop orderedBy              Optional <@ fun x -> x.OrderedBy @>;

            // Common properties for data nodes
            prop any     Optional <@ fun x -> x.Description @>;
            prop any     Optional <@ fun x -> x.Reference @>;
            prop status  Optional <@ fun x -> x.Status @>;

            // Typedefs
            child ptypedef Many (fun _ _ _ -> Ok());

            // Data statements
            chld pcontainer Many <@ fun x -> x.DataNodes @>;
            chld pleaf      Many <@ fun x -> x.DataNodes @>;
            chld pleaflist  Many <@ fun x -> x.DataNodes @>;
            chld plist      Many <@ fun x -> x.DataNodes @>;
        ])

let pmodulerevision : StatementSpec<YANGModuleRevision> =
    createSpec
        "revision"
        YANGModuleRevision
        (arg Statements.dateArg)
        (anyOf [
            prop any Optional <@ fun x -> x.Description @>;
            prop any Optional <@ fun x -> x.Reference @>;
        ])

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
                    Ok()
                );
                property "prefix" unqualifiedIdentifier Required (fun node value ctx ->
                    node.Prefix <- value
                    ctx.Scope.Prefixes.Add(node.Prefix, node)
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
                // Typedefs
                child ptypedef Many (fun x t _ -> x.ExportedTypes.Add(t.Name, t); Ok());

                // Data statements
                chld pcontainer Many <@ fun x -> x.DataNodes @>;
                chld pleaf      Many <@ fun x -> x.DataNodes @>;
                chld pleaflist  Many <@ fun x -> x.DataNodes @>;
                chld plist      Many <@ fun x -> x.DataNodes @>;
            ]
        ])