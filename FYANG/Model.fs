module FYANG.Model

open System
open System.Collections.Generic
open FYANG.Statements

/// Representation of all the possible errors that can happen
/// during the parsing and validation of a YANG model
[<StructuredFormatDisplay("{Text}")>]
type SchemaError =
    
    // Generic syntax error
    | ParserError of string
    
    // YANG Version
    | UnsupportedYangVersion of Statement * string
    
    // Statement argument
    | ArgumentExpected of Statement
    | NoArgumentExpected of Statement
    | ArgumentParserError of Statement * string
    
    // Statement cardinality
    | ExpectedStatement of Statement * string
    | UnexpectedStatement of Statement
    | MissingRequiredStatement of Statement * string
    | TooManyInstancesOfStatement of Statement

    // Namespaces and modules
    | AlreadyUsedModuleName of Statement * YANGModule
    | UnknownPrefix of Statement
    | AlreadyUsedPrefix of Statement * Statement
    | AlreadyUsedNamespace of Statement * YANGModule

    with

        member this.Text = this.ToString()

        override this.ToString() =
            match this with
            | ParserError(x) -> x
            | _ ->
                let (stmt, msg) =
                    match this with
                    | UnsupportedYangVersion(x, y) ->
                        x, (sprintf "Unsupported YANG version %s." y)
                    | ArgumentExpected(x) ->
                        x, (sprintf "Statement \"%s\" expects an argument." x.Name)
                    | NoArgumentExpected(x) ->
                        x, (sprintf "Statement \"%s\" does not expect an argument." x.Name)
                    | ArgumentParserError(x, y) ->
                        x, (sprintf "Error parsing argument for statement \"%s\":%s%s" x.Name Environment.NewLine y)
                    | ExpectedStatement(x, y) ->
                        x, (sprintf "Expected \"%s\" statement, but got \"%s\"." x.Name y)
                    | UnexpectedStatement(x) ->
                        x, (sprintf "Unexpected statement \"%s\"." x.Name)
                    | MissingRequiredStatement(x, y) ->
                        x, (sprintf "Missing required statement \"%s\"." y)
                    | TooManyInstancesOfStatement(x) ->
                        x, (sprintf "Too many instances of the \"%s\" statement." x.Name)
                    | AlreadyUsedModuleName(x, y) ->
                        let s: Statement = y.OriginalStatement
                        x, (sprintf "Module name already used by module at %A." s.Position)
                    | UnknownPrefix(x) ->
                        x, (sprintf "Unknown prefix \"%s\"." x.Prefix.Value)
                    | AlreadyUsedPrefix(x, y) ->
                        x, (sprintf "Prefix already registered. See statement at %d:%d" y.Position.Line y.Position.Column)
                    | AlreadyUsedNamespace(x, y) ->
                        let s: Statement = y.OriginalStatement
                        x, (sprintf "Namespace already registered by module \"%A\" (%A)." y.Name s.Position)
                    | _ ->
                        invalidOp "Should never be reached"
                
                sprintf "Statement \"%s\" (%A): %s" stmt.Name stmt.Position msg

/// Namespace used in a YANG model.
/// Namespaces are strictly tied to the module that defined them.
and [<StructuredFormatDisplay("{{{Uri}}}")>] YANGNamespace =
    {
        Module: YANGModule;
        Uri: Uri;
    }
    with
        static member Empty = { Module = null; Uri = null }

/// Name of YANGNode.
/// Names can't be simple strings because they need to be qualified by a namespace.
and [<StructuredFormatDisplay("{Namespace}{Name}")>] YANGName =
    {
        Namespace: YANGNamespace;
        Name: string;
    }

/// Base class for all YANG nodes
and [<AbstractClass>][<AllowNullLiteral>] YANGNode(s: Statement) =
    member this.OriginalStatement = s



// -------------------------------------------------------------------
// Implementation of all the supported nodes
// -------------------------------------------------------------------

and [<AllowNullLiteral>] YANGModule(s: Statement, unqualifiedName: string) =
    inherit YANGNode(s)
    
    member this.Name
        with get() =
            if this.Namespace = YANGNamespace.Empty then
                invalidOp "Cannot retrive fully qualified name before setting the `Namespace` property."
            {
                Namespace = this.Namespace;
                Name = unqualifiedName;
            }

    member val Namespace = YANGNamespace.Empty with get, set
    member val Prefix: string = null with get, set
    member val Contact: string = null with get, set
    member val Organization: string = null with get, set
    member val Description: string = null with get, set
    member val Reference: string = null with get, set


    member this.Revisions = ResizeArray<YANGModuleRevision>()
    member this.Containers = ResizeArray<YANGContainer>()
    member this.Leafs = ResizeArray<YANGLeaf>()
    member this.LeafLists = ResizeArray<YANGLeafList>()
    member this.Lists = ResizeArray<YANGList>()

and YANGModuleRevision(s: Statement, date: DateTime) =
    inherit YANGNode(s)

    member this.Date = date

and YANGContainer(s: Statement, name: YANGName) =
    inherit YANGNode(s)

    member this.Name = name
    
and YANGLeaf(s: Statement, name: YANGName) =
    inherit YANGNode(s)

    member this.Name = name

and YANGLeafList(s: Statement, name: YANGName) =
    inherit YANGNode(s)

    member this.Name = name

and YANGList(s: Statement, name: YANGName) =
    inherit YANGNode(s)

    member this.Name = name