namespace YANGParser.Tests

open System
open Xunit
open FParsec
open YANGParser
open YANGParser.Tests.Utils

module Whitespace =

    [<Fact>]
    let ``Consecutive spaces are treated as one`` () =
        AssertSuccess (ws >>. eof) " \t \n \r \n  "
        AssertSuccess (ws1 >>. eof) " \t \n \r \n  "

    [<Fact>]
    let ``Line comments`` () =
        AssertSuccess (ws >>. eof) "// Comment"

    [<Fact>]
    let ``Line comments are allowed to start at the middle of a line`` () =
        AssertSuccess (stringLiteral >>. ws >>. eof) "test // Comment"

    [<Fact>]
    let ``Block comments`` () =
        AssertSuccess (ws >>. eof) "/* Comment */"

    [<Fact>]
    let ``Unquoted strings end when a comment starts`` () =
        AssertParses stringLiteral "test//roba" "test"
        AssertParses stringLiteral "test/*roba*/" "test"
        AssertParses stringLiteral "test/roba" "test/roba" // A single slash is fine

    [<Fact>]
    let ``Multiple comments are parsed as one whitespace`` () =
        AssertSuccess (ws >>. eof) "/* c1 *//* c2 */"

    [<Fact>]
    let ``Comments and whitespace are treated as one`` () =
        AssertSuccess (ws >>. eof) "/* \t c1 */ \n /* \n c2 \n */"