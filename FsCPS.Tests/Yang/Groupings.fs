namespace FsCPS.Tests.Yang

open System
open Xunit
open FsCPS
open FsCPS.Tests
open FsCPS.Yang

module Groupings =
    
    [<Fact>]
    let ``Groupings at the top level are exported`` () =
        let m =
            Utils.CreateModule """
                grouping my-grouping {
                    leaf l {
                        type int32;
                    }
                }
            """
            |> Result.okOrThrow (string >> invalidOp)
        Assert.Equal(1, m.ExportedGroupings.Count)
        let g = m.ExportedGroupings.Values |> Seq.head
        Assert.Equal("my-grouping", g.Name.Name)
        Assert.Equal(1, g.DataNodes.Count)

    [<Fact>]
    let ``Groupings inside other data nodes are not exported`` () =
        let m =
            Utils.CreateModule """
                container x {
                    grouping my-grouping {
                        leaf l {
                            type int32;
                        }
                    }
                }
            """
            |> Result.okOrThrow (string >> invalidOp)
        Assert.Equal(0, m.ExportedGroupings.Count)

    [<Fact>]
    let ``Groupings allow forward references`` () =
        let m =
            Utils.CreateModule """
                uses my-grouping;

                grouping my-grouping {
                    leaf l {
                        type int32;
                    }
                }
            """
            |> Result.okOrThrow (string >> invalidOp)
        Assert.Equal(1, m.DataNodes.Count)
        Assert.Equal("l", m.DataNodes |> Seq.head |> (fun x -> x.Name.Name))

    [<Fact>]
    let ``Groupings inside data nodes are scoped to that node`` () =
        let m =
            Utils.CreateModule """
                container x {
                    grouping my-grouping {
                        leaf l {
                            type int32;
                        }
                    }
                }

                uses my-grouping;
            """
        match m with
        | Error [ UnresolvedGroupingRef(_, "my-grouping") ] -> ()
        | _ -> invalidOp "Expected grouping resolution failure."

    [<Fact>]
    let ``Groupings can be used once`` () =
        let m =
            Utils.CreateModule """
                grouping my-grouping {
                    leaf l {
                        type int32;
                    }
                }

                uses my-grouping;
            """
            |> Result.okOrThrow (string >> invalidOp)
        Assert.Equal(1, m.DataNodes.Count)
        Assert.Equal("l", m.DataNodes |> Seq.head |> (fun x -> x.Name.Name))

    [<Fact>]
    let ``Groupings can be used twice`` () =
        let m =
            Utils.CreateModule """
                grouping my-grouping {
                    leaf l {
                        type int32;
                    }
                }

                uses my-grouping;

                container x {
                    uses my-grouping;
                }
            """
            |> Result.okOrThrow (string >> invalidOp)
        Assert.Equal(2, m.DataNodes.Count)
        Assert.Equal("l", m.DataNodes |> Seq.head |> (fun x -> x.Name.Name))
        let container = (m.DataNodes |> Seq.item 1) :?> YANGContainer
        Assert.Equal("x", container.Name.Name)
        Assert.Equal(1, container.DataNodes.Count)
        Assert.Equal("l", container.DataNodes |> Seq.head |> (fun x -> x.Name.Name))

    [<Fact>]
    let ``Groupings inclusions can be recursive`` () =
        let m =
            Utils.CreateModule """
                grouping my-grouping1 {
                    uses my-grouping2;
                }

                grouping my-grouping2 {
                    leaf l {
                        type int32;
                    }
                }

                uses my-grouping1;
            """
            |> Result.okOrThrow (string >> invalidOp)
        Assert.Equal(1, m.DataNodes.Count)
        Assert.Equal("l", m.DataNodes |> Seq.head |> (fun x -> x.Name.Name))

    [<Fact>]
    let ``Groupings inclusions respect node order`` () =
        let m =
            Utils.CreateModule """
                grouping my-grouping1 {
                    leaf l1 {
                        type int32;
                    }
                }

                grouping my-grouping2 {
                    leaf l2 {
                        type int32;
                    }
                }

                leaf a { type int32; }
                uses my-grouping1;
                leaf b { type int32; }
                uses my-grouping2;
                leaf c { type int32; }
            """
            |> Result.okOrThrow (string >> invalidOp)
        Assert.Equal([ "a"; "l1"; "b"; "l2"; "c" ], m.DataNodes |> Seq.map (fun x -> x.Name.Name))

    [<Fact>]
    let ``Empty groupings are valid`` () =
        let m =
            Utils.CreateModule """
                grouping my-grouping1 { }

                grouping my-grouping2 { }

                leaf a { type int32; }
                uses my-grouping1;
                leaf b { type int32; }
                uses my-grouping2;
                leaf c { type int32; }
            """
            |> Result.okOrThrow (string >> invalidOp)
        Assert.Equal([ "a"; "b"; "c" ], m.DataNodes |> Seq.map (fun x -> x.Name.Name))
    
    [<Fact>]
    let ``Nodes of a grouping change namespace to then one of the destination module when used`` () =
        
        // Create the module that exports the grouping
        let lib =
            YANGParser.ParseModule """
                module my-lib {
                    namespace "http://example.com/my-lib";
                    prefix "lib";

                    grouping my-grouping {
                        leaf l {
                            type int32;
                        }
                    }
                }
            """
            |> Result.okOrThrow (string >> invalidOp)

        // Create the module that uses the grouping and "link" them together
        let opts = YANGParserOptions(ResolveImport = fun _ _ -> Ok lib)
        let m =
            YANGParser.ParseModule("""
                module my-module {
                    namespace "http://example.com/my-module";
                    prefix "m";

                    import my-lib {
                        prefix "lib";
                    }

                    uses lib:my-grouping;
                }
            """, opts)
            |> Result.okOrThrow (string >> invalidOp)

        // Check that the namespace of the nodes is correct
        Assert.Equal([ "l" ], m.DataNodes |> Seq.map (fun x -> x.Name.Name))
        Assert.Equal(m.Namespace, m.DataNodes.[0].Name.Namespace)

    [<Fact>]
    let ``Grouping references augmentations require relative paths`` () =
        let m =
            Utils.CreateModule """
                grouping my-grouping {}

                uses my-grouping {
                    augment /cont {
                        leaf l {
                            type int32;
                        }
                    }
                }
            """
        match m with
        | Error [ RelativeNodePathRequired _ ] -> ()
        | _ -> invalidOp "Expected grouping augmentation failure."

    [<Fact>]
    let ``Grouping references can be augmented`` () =
        let m =
            Utils.CreateModule """
                grouping my-grouping {
                    container cont {
                        leaf l1 {
                            type int32;
                        }
                    }
                }

                uses my-grouping {
                    augment cont {
                        leaf l2 {
                            type int32;
                        }
                    }
                }
            """
            |> Result.okOrThrow (string >> invalidOp)
        Assert.Equal(1, m.DataNodes.Count)
        let c = m.DataNodes.[0] :?> YANGContainer
        Assert.Equal("cont", c.Name.Name)
        Assert.Equal(2, c.DataNodes.Count)
        Assert.Equal("l1", c.DataNodes.[0].Name.Name)
        Assert.Equal("l2", c.DataNodes.[1].Name.Name)