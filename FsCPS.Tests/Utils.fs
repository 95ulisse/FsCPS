namespace FsCPS.Tests

open FsCPS
open FsCPS.Yang
open FParsec
open Xunit
open Xunit.Sdk

module Utils =

    let rec compareStatements (s1: Statement) (s2: Statement) =
        // Note that we don't compare the position.
        s1.Name = s2.Name &&
        s1.Argument = s2.Argument &&
        s1.Children.Count = s2.Children.Count &&
        [0 .. (s1.Children.Count - 1)]
            |> List.forall (fun i -> compareStatements s1.Children.[i] s2.Children.[i])

    let AssertSuccess parser str =
        match run parser str with
        | Success _ -> ()
        | Failure(e, _, _) -> raise (XunitException e)

    let AssertFailure parser str =
        match run parser str with
        | Success(res, _, _) -> raise (XunitException (sprintf "Parser should not have parsed: %s as %A" str res))
        | Failure _ -> ()

    let AssertParses parser str res =
        match run parser str with
        | Success(r, _, _) ->
            if r <> res then
                raise (XunitException (sprintf "Expected %A. Got %A." res r))
        | Failure(e, _, _) -> raise (XunitException e)

    let AssertParsesStmt parser str res =
        match run parser str with
        | Success(r, _, _) ->
            if not (compareStatements r res) then
                raise (XunitException (sprintf "Expected %A. Got %A." res r))
        | Failure(e, _, _) -> raise (XunitException e)

    let AssertOk res =
        match res with
        | Result.Ok x -> x
        | Result.Error e -> raise (XunitException (sprintf "Expected Ok, but got Error %A." e))

    let CreateModule inner =
        YANGParser.ParseModule("""
            module test {
                namespace "http://example.com/test-module";
                prefix test;
                """ + inner + """
            }
        """)

    let AssertCPSObjectEquals (a: CPSObject) (b: CPSObject) =
        // Compares the keys
        if a.Key.Key <> b.Key.Key then
            raise (XunitException (sprintf "Expected object with key %s, but got %s." a.Key.Key b.Key.Key))
        
        // Compares the attributes
        Assert.True(a.Attributes = b.Attributes)