namespace FsCPS.Yang

open FsCPS
open FsCPS.Yang.Model
open FsCPS.Yang.Parser

type YANGParser() =

    member __.ParseModule str =

        // Parses the string as a tree of statements
        match FParsec.CharParsers.run Statements.root str with
        | FParsec.CharParsers.Failure(e, _, _) -> Error([ ParserError(e) ])
        | FParsec.CharParsers.Success(statement, _, _) ->

            // Transforms the statements to the actual schema tree
            let ctx = SchemaParserContext(statement)
            pmodule.Parser ctx