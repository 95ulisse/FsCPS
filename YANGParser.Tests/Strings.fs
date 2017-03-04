namespace YANGParser.Tests

open System
open Xunit
open FParsec
open YANGParser
open YANGParser.Tests.Utils

module Strings =

    [<Fact>]
    let ``Parses unquoted strings`` () =
        AssertParses stringLiteral "test" "test"
        AssertParses stringLiteral "test test" "test"
        AssertParses stringLiteral "test;test" "test"
        AssertParses stringLiteral "test{test}" "test"

    [<Fact>]
    let ``Parses single quoted strings`` () =
        AssertParses stringLiteral "'test'" "test"
    
    [<Fact>]
    let ``Parses double quoted strings`` () =
        AssertParses stringLiteral "\"test\"" "test"

    [<Fact>]
    let ``Escapes are parsed only in double quoted strings`` () =
        AssertParses stringLiteral "'\\\\\\nA\\r\\t\\\"B'" "\\\\\\nA\\r\\t\\\"B"
        AssertParses stringLiteral "\"A\\\\A\\tA\\nA\\rA\\\"A\"" "A\\A\tA\nA\nA\"A"

    [<Fact>]
    let ``Strings can be concatenated with a "+"`` () =
        AssertParses stringLiteral "\"hel\" + \"lo\"" "hello"
        AssertParses stringLiteral "'hel' + 'lo'" "hello"
        AssertParses stringLiteral "'hel' + \"lo\"" "hello"
        AssertParses stringLiteral "\"hel\" + 'lo'" "hello"

    [<Fact>]
    let ``Multiline strings are allowed`` () =
        AssertParses stringLiteral "\"first\nsecond\"" "first\nsecond"
        AssertParses stringLiteral "'first\nsecond'" "first\nsecond"

    [<Fact>]
    let ``Whitespace on multiline double quoted strings is stripped`` ()  =
        let secondString = stringLiteral >>. spaces1 >>. stringLiteral

        let str1 =
            "padding \"first line  \n" +
            "           second line\""
        AssertParses secondString str1 "first line\n  second line"

        let str2 =
            "padding \"first line  \n" +
            "     second line\""
        AssertParses secondString str2 "first line\nsecond line"

        let str3 =
            "padding \"first line  \n" +
            "         second line\""
        AssertParses secondString str3 "first line\nsecond line"

    [<Fact>]
    let ``Tabs are counted as 8 spaces while stripping leading whitespace`` ()  =
        let secondString = stringLiteral >>. spaces1 >>. stringLiteral

        let str1 =
            "padding \"first line  \n" +
            " \t  second line\""
        AssertParses secondString str1 "first line\n  second line"

        let str2 =
            "padding \"first line  \n" +
            "\tsecond line\""
        AssertParses secondString str2 "first line\nsecond line"

        let str3 =
            "padding \"first line  \n" +
            " \tsecond line\""
        AssertParses secondString str3 "first line\nsecond line"

    [<Fact>]
    let ``Unquoted strings end when a comment starts`` () =
        AssertParses stringLiteral "test//roba" "test"
        AssertParses stringLiteral "test/*roba*/" "test"
        AssertParses stringLiteral "test/roba" "test/roba" // A single slash is fine