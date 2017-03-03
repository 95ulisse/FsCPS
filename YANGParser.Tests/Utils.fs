namespace YANGParser.Tests

open FParsec
open Xunit
open Xunit.Sdk

module Utils =

    let AssertSuccess parser str =
        match run parser str with
        | Success _ -> ()
        | Failure(e, _, _) -> raise (XunitException(e))

    let AssertFailure parser str =
        match run parser str with
        | Success(res, _, _) -> raise (XunitException(sprintf "Parser should not have parsed: %s as %A" str res))
        | Failure _ -> ()