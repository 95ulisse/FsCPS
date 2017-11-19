namespace FsCPS.Tests.Yang

open System
open Xunit
open FsCPS
open FsCPS.Yang

module UnknownStatements =

    let private doParse cb str =
        let opts = YANGParserOptions(UnknownStatement = cb)
        YANGParser.ParseModule("""
            module test {
                namespace "http://example.com/test-module";
                prefix test;
                """ + str + """
            }
        """, opts)
        |> Result.okOrThrow (string >> invalidOp)


 
    [<Fact>]
    let ``Unknown statements call the callback provided in the options`` () =
        let called = ref false
        doParse
            (fun _ -> called := true; Ok ())
            "fake-statement fake-argument;"
        |> ignore
        Assert.True(!called)


    [<Fact>]
    let ``Parser can be stopped by returning an error`` () =
        Assert.Throws<InvalidOperationException> (fun () ->
            doParse
                (fun _ -> Error [ ParserError "An error." ])
                "fake-statement fake-argument;"
            |> ignore
        )


    [<Fact>]
    let ``Parsing continues if the callback does not return an error`` () =
        let m =
            doParse
                (fun _ -> Ok ())
                """
                    leaf l1 { type int32; }
                    fake-statement fake-argument;
                    leaf l2 { type int32; }
                """
        Assert.Equal(2, m.DataNodes.Count)


    [<Fact>]
    let ``Parsing continues if the callback does not return an error, even in nested statements`` () =
        let m =
            doParse
                (fun _ -> Ok ())
                """
                    container c {
                        leaf l1 { type int32; }
                        fake-statement fake-argument;
                        leaf l2 { type int32; }
                    }
                    leaf l3 { type int32; }
                """
        Assert.Equal(2, m.DataNodes.Count)
        let c = m.DataNodes.[0] :?> YANGContainer
        Assert.Equal(2, c.DataNodes.Count)