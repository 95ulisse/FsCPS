module FYANG.Parser

open System.Collections.Generic
open FYANG.Statements
open FYANG.Model

/// Context of the parsing operation.
/// Contains a pointer to the current statement and other useful informations.
/// It also implements a stack-like semantics to aid recursion.
type SchemaParserContext(rootStatement: Statement) =

    let _parentStack = Stack<YANGNode>()

    member val RootStatement: Statement = rootStatement
    member val Statement = rootStatement with get, set
    member this.GetParentNode<'T when 'T :> YANGNode>() =
        _parentStack.Peek() :?> 'T

    member this.PushParent n =
        _parentStack.Push(n)

    member this.PopParent() =
        _parentStack.Pop()


/// A parser is a function from a parsing context to a result or an error list.
type SchemaParser<'T> = SchemaParserContext -> Result<'T, SchemaError list>

type Cardinality =
    | Optional
    | Required
    | Many
    | Many1

type ChildStatementDescription<'T> = {
    Name: string;
    Cardinality: Cardinality;
    Parser: SchemaParser<'T>;
}

type ChildStatementHandler<'TNode, 'TArg> = 'TNode -> 'TArg -> Result<unit, SchemaError list>

let getOrDefault key def map =
    match Map.tryFind key map with
    | Some(x) -> x
    | None -> def

let nodeWithArg ctor =
    fun (s: Statement) ->
        if s.Argument.IsNone then
            Error([ ArgumentExpected(s.Name) ])
        else
            Ok(ctor s)

let nodeWithoutArg ctor =
    fun (s: Statement) ->
        if s.Argument.IsSome then
            Error([ NoArgumentExpected(s.Name) ])
        else
            Ok(ctor s)

/// Helper function used to check if the cardinality of a statement is respected
let checkCardinality desc count =
    if count < 0 then
        invalidArg "count" "Invalid negative count"
    match desc.Cardinality with
    | Optional ->
        if count <= 1 then
            None
        else
            Some([ TooManyInstancesOfStatement(desc.Name) ])
    | Required ->
        if count = 0 then
            Some([ MissingRequiredStatement(desc.Name) ])
        else if count > 1 then
            Some([ TooManyInstancesOfStatement(desc.Name) ])
        else
            None
    | Many ->
        None
    | Many1 ->
        if count = 0 then
            Some([ MissingRequiredStatement(desc.Name) ])
        else
            None

/// Builds a `ChildStatementDescription` for a statement that will be mapped
/// to a property of a YANG node.
let property name cardinality (handler: ChildStatementHandler<'a, string option>) : ChildStatementDescription<'a> =

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
                Error([ ExpectedStatement(name, ctx.Statement.Name) ])
            else
                // Executes the handler
                match handler (ctx.GetParentNode()) ctx.Statement.Argument with
                | Ok(_) -> Ok(ctx.GetParentNode())
                | Error(l) -> Error(l)

    {
        Name = name;
        Cardinality = cardinality;
        Parser = parser;
    }

/// Builds a `ChildStatementDescription` for a statement that will construct other YANG nodes.
let child name (parser: SchemaParser<'b>) cardinality (handler: ChildStatementHandler<'a, 'b>) : ChildStatementDescription<'a> =
    let parser : SchemaParser<_> =
        fun ctx ->
            // Check that the current statement has the expected name
            if ctx.Statement.Name <> name then
                Error([ ExpectedStatement(name, ctx.Statement.Name) ])
            else
                // Parses the child
                match parser ctx with
                | Error(l) -> Error(l)
                | Ok(child) ->
                    let parent = ctx.GetParentNode()
                    match handler parent child with
                    | Ok(_) -> Ok(parent)
                    | Error(l) -> Error(l)

    {
        Name = name;
        Cardinality = cardinality;
        Parser = parser;
    }

/// Creates a parser for a generic YANG node, providing a name, a constructor function
/// and a description of the allowed children.
let createParser name (ctor: Statement -> Result<'a, SchemaError list>) (childrenDesc: ChildStatementDescription<'a> list) : SchemaParser<'a> =
    let childrenMap =
        childrenDesc
        |> List.map (fun desc -> (desc.Name, desc))
        |> Map.ofList
    fun ctx ->
        // Check that the current statement has the expected name
        if ctx.Statement.Name <> name then
            Error([ ExpectedStatement(name, ctx.Statement.Name) ])
        else
            // Constructs the node
            match ctor ctx.Statement with
            | Ok(node) ->

                let currentStatement = ctx.Statement
                ctx.PushParent(node)

                // Cycles all the child statements and parses them.
                // We also keep track of how many times we encounter each child
                // to be sure that the cardinality is respected.
                let mutable childrenCount = Map.empty
                let mutable result =
                    currentStatement.Children |> List.fold (fun acc c ->
                        // Note that the first error gets propagated,
                        // and the parsers for the other children are not called.
                        match acc with
                        | Ok(_) ->
                            ctx.Statement <- c
                            match Map.tryFind c.Name childrenMap with
                            | Some(childDesc) ->

                                // Checks if parsing the child would exceed the cardinality
                                let childCount = childrenCount |> getOrDefault childDesc.Name 0
                                match checkCardinality childDesc (childCount + 1) with
                                | Some(l) -> Error(l)
                                | None ->
                                    // Executes the parser for the child
                                    childrenCount <- childrenCount.Add(childDesc.Name, childCount + 1)
                                    match childDesc.Parser ctx with
                                    | Ok(_) -> Ok(node)
                                    | Error(l) -> Error(l)

                            | None -> Error([ UnexpectedStatement(c.Name) ])
                        | Error(_) -> acc
                    ) (Ok(node))
                
                // Checks that all the required children are present
                match result with
                | Error(_) -> ()
                | Ok(_) ->
                    let requiredChildrenErrors =
                        childrenDesc
                        |> List.filter (fun c -> c.Cardinality = Required)
                        |> List.fold (fun acc c ->
                            match checkCardinality c (childrenCount |> getOrDefault c.Name 0) with
                            | Some(l) -> acc @ l
                            | None -> acc
                        ) []
                    if requiredChildrenErrors <> [] then
                        result <- Error(requiredChildrenErrors)
                
                // Restores the parsing context and returns
                ctx.PopParent() |> ignore
                ctx.Statement <- currentStatement
                result

            | Error(l) -> Error(l)

// ------------------------------------------------------------------

/// Properties common to all statements
let commonProperties () =
    [
        property "description" Optional (fun node value -> node.Description <- value.Value; Ok());
        property "reference"   Optional (fun node value -> node.Reference <- value.Value; Ok());
    ]

/// Creates a parser for a node expecting an argument.
/// Automatically includes all the common properties.
let createParserWithArg name ctor children =
    createParser name (nodeWithArg ctor) (children @ (commonProperties ()))

/// Creates a parser for a node expecting no argument.
/// Automatically includes all the common properties.
let createParserWithoutArg name ctor children =
    createParser name (nodeWithoutArg ctor) (children @ (commonProperties ()))

// ------------------------------------------------------------------

let pleaf : SchemaParser<YANGLeaf> =
    createParserWithArg
        "leaf"
        YANGLeaf
        [
            property "default" Optional (fun node value -> node.Default <- value.Value; Ok());
        ]

let pmodulerevision : SchemaParser<YANGModuleRevision> =
    createParserWithArg
        "revision"
        YANGModuleRevision
        []

let pmodule : SchemaParser<YANGModule> =
    createParserWithArg
        "module"
        YANGModule
        [
            property "yang-version" Optional (fun node value -> if value.Value <> "1" then Error([ UnsupportedYangVersion value.Value ]) else Ok());
            property "namespace"    Required (fun node value -> node.Namespace <- value.Value; Ok());

            child "revision" pmodulerevision Many (fun node child -> node.Revisions.Add(child); Ok());
            child "leaf" pleaf Many (fun node child -> node.Leafs.Add(child); Ok());
        ]