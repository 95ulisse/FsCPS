module FYANG.Model

open System
open System.Collections.Generic
open FYANG.Statements

/// Representation of all the possible errors that can happen
/// during the parsing and validation of a YANG model
type SchemaError =
    | ParserError of string
    | UnsupportedYangVersion of string
    | ArgumentExpected of string
    | NoArgumentExpected of string
    | ExpectedStatement of string * string
    | UnexpectedStatement of string
    | MissingRequiredStatement of string
    | TooManyInstancesOfStatement of string

    with
        override this.ToString() =
            match this with
            | ParserError(x) -> x
            | UnsupportedYangVersion(x) -> sprintf "Unsupported YANG version %s." x
            | ArgumentExpected(x) -> sprintf "Statement \"%s\" expects an argument." x
            | NoArgumentExpected(x) -> sprintf "Statement \"%s\" does not expect an argument." x
            | ExpectedStatement(x, y) -> sprintf "Expected \"%s\" statement, but got \"%s\"." x y
            | UnexpectedStatement(x) -> sprintf "Unexpected statement \"%s\"." x
            | MissingRequiredStatement(x) -> sprintf "Missing required statement \"%s\"." x
            | TooManyInstancesOfStatement(x) -> sprintf "Too many instances of the \"%s\" statement." x

/// Base class for all YANG nodes
[<AbstractClass>]
type YANGNode(s: Statement) =
    member this.OriginalStatement = s
    member val Description: string = null with get, set
    member val Reference: string = null with get, set

/// Schema resulting from a full YANG model parsing
type Schema = {
    Module: YANGModule;
    // validator: XDocument -> ValidationResult;
}

// -------------------------------------------------------------------
// Implementation of all the supported nodes
// -------------------------------------------------------------------

and YANGModule(s: Statement) =
    inherit YANGNode(s)

    member this.Name = s.Argument.Value
    member val Contact: string = null with get, set
    member val Namespace: string = null with get, set
    member val Organization: string = null with get, set
    member val Prefix: string = null with get, set
    member val Reference: string = null with get, set

    member this.Revisions = ResizeArray<YANGModuleRevision>()
    member this.Leafs = ResizeArray<YANGLeaf>()

and YANGModuleRevision(s: Statement) =
    inherit YANGNode(s)

    member this.Name = s.Argument.Value

and YANGLeaf(s: Statement) =
    inherit YANGNode(s)

    member this.Name = s.Argument.Value
    member val Description: string = null with get, set
    member val Default: string = null with get, set