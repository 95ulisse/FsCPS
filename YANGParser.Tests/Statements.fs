namespace YANGParser.Tests

open System
open Xunit
open FParsec
open YANGParser
open YANGParser.Tests.Utils

module Statements =

    let s ns name argument children = {
        ns = ns;
        name = name;
        argument = argument;
        children = children;
    }

    [<Fact>]
    let ``Namespaces are optional`` () =
        AssertParses statement "name arg;" (s None "name" (Some "arg") [])
        AssertParses statement "ns:name arg;" (s (Some "ns") "name" (Some "arg") [])

    [<Fact>]
    let ``Arguments are optional`` () =
        AssertParses statement "name;" (s None "name" None [])

    [<Fact>]
    let ``Zero children statement`` () =
        AssertParses statement
            "name {  };"
            (s None "name" None [])

    [<Fact>]
    let ``One child statement`` () =
        AssertParses statement
            "name { name2; };"
            (s None "name" None [ (s None "name2" None []) ])

    [<Fact>]
    let ``Two children statements`` () =
        AssertParses statement
            "name { name2; name3; };"
            (s None "name" None [ (s None "name2" None []); (s None "name3" None []) ])

    [<Fact>]
    let ``Nested children statements`` () =
        AssertParses statement
            "name { name2; name3 { nameX; } name4 { nameY; } };"
            (s None "name" None [
                (s None "name2" None []);
                (s None "name3" None [
                    (s None "nameX" None [])
                ]);
                (s None "name4" None [
                    (s None "nameY" None [])
                ])
            ])

    [<Fact>]
    let ``Less whitespace possible`` () =
        AssertParses statement
            "parent{child1;child2;}"
            (s None "parent" None [
                (s None "child1" None []);
                (s None "child2" None [])
            ])

    [<Fact>]
    let ``Whitespace everywhere`` () =
        AssertParses statement
            "name 'arg arg' \n{ name2 \n arg2 \n ; \n }\n"
            (s None "name" (Some "arg arg") [ (s None "name2" (Some "arg2") []) ])
