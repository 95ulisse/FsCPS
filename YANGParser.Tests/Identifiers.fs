namespace YANGParser.Tests

open System
open Xunit
open YANGParser
open YANGParser.Tests.Utils

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