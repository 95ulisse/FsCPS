namespace FYANG.Tests

open System
open FParsec
open Xunit
open FYANG.Statements
open FYANG.Tests.Utils

module Identifiers =

    [<Fact>]
    let ``Identifiers can start with "_"`` () =
        AssertSuccess id "_a"

    [<Fact>]
    let ``Identifiers can start with a letter`` () =
        AssertSuccess id "abc"

    [<Fact>]
    let ``Identifiers cannot start with a digit`` () =
        AssertFailure id "123"

    [<Fact>]
    let ``Identifiers cannot start with X, M or L`` () =
        AssertFailure id "X123"
        AssertFailure id "x123"
        AssertFailure id "M123"
        AssertFailure id "m123"
        AssertFailure id "L123"
        AssertFailure id "l123"



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



module FullStatements =

    let s name argument children =
        let stmt =
            Statement(
                name,
                { Line = 0L; Column = 0L },
                Argument = argument
            )
        children |> List.iter (fun c -> stmt.Children.Add(c))
        stmt

    [<Fact>]
    let ``Namespaces are optional`` () =
        AssertParsesStmt statement "name arg;" (s "name" (Some "arg") [])

    [<Fact>]
    let ``Arguments are optional`` () =
        AssertParsesStmt statement "name;" (s "name" None [])

    [<Fact>]
    let ``Zero children statement`` () =
        AssertParsesStmt statement
            "name {  };"
            (s "name" None [])

    [<Fact>]
    let ``One child statement`` () =
        AssertParsesStmt statement
            "name { name2; };"
            (s "name" None [ (s "name2" None []) ])

    [<Fact>]
    let ``Two children statements`` () =
        AssertParsesStmt statement
            "name { name2; name3; };"
            (s "name" None [ (s "name2" None []); (s "name3" None []) ])

    [<Fact>]
    let ``Nested children statements`` () =
        AssertParsesStmt statement
            "name { name2; name3 { nameX; } name4 { nameY; } };"
            (s "name" None [
                (s "name2" None []);
                (s "name3" None [
                    (s "nameX" None [])
                ]);
                (s "name4" None [
                    (s "nameY" None [])
                ])
            ])

    [<Fact>]
    let ``Less whitespace possible`` () =
        AssertParsesStmt statement
            "parent{child1;child2;}"
            (s "parent" None [
                (s "child1" None []);
                (s "child2" None [])
            ])

    [<Fact>]
    let ``Whitespace everywhere`` () =
        AssertParsesStmt statement
            "name 'arg arg' \n{ name2 \n arg2 \n ; \n }\n"
            (s "name" (Some "arg arg") [ (s "name2" (Some "arg2") []) ])

    [<Fact>]
    let ``Keyword names can start with X, M or L`` () =
        AssertParsesStmt statement
            "module {}"
            (s "module" None [])
        AssertParsesStmt statement
            "lestatement {}"
            (s "lestatement" None [])
        AssertParsesStmt statement
            "xstatement {}"
            (s "xstatement" None [])