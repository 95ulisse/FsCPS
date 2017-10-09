namespace FsCPS.Tests.Yang

open System
open Xunit
open FsCPS
open FsCPS.Tests
open FsCPS.Yang

module Augments =

    [<Fact>]
    let ``Augmentations add data nodes to the target`` () =
        let m =
            Utils.CreateModule """
                container cont {
                    leaf first { type int32; }
                }

                augment /cont {
                    leaf second { type int64; }
                }
            """
            |> Result.okOrThrow (string >> invalidOp)
        Assert.Equal(1, m.Augmentations.Count)
        Assert.Equal(1, m.DataNodes.Count)
        let c = m.DataNodes.[0] :?> YANGContainer
        Assert.Equal(2, c.DataNodes.Count)
        Assert.Equal("first", c.DataNodes.[0].Name.Name)
        Assert.Equal("second", c.DataNodes.[1].Name.Name)

    [<Fact>]
    let ``Deep path`` () =
        let m =
            Utils.CreateModule """
                container cont1 {
                    container cont2 {
                        container cont3 {
                            leaf first { type int32; }
                        }
                    }
                }

                augment /cont1/cont2/cont3 {
                    leaf second { type int64; }
                }
            """
            |> Result.okOrThrow (string >> invalidOp)
        Assert.Equal(1, m.Augmentations.Count)
        Assert.Equal(1, m.DataNodes.Count)
        let c1 = m.DataNodes.[0] :?> YANGContainer
        Assert.Equal(1, c1.DataNodes.Count)
        let c2 = c1.DataNodes.[0] :?> YANGContainer
        Assert.Equal(1, c2.DataNodes.Count)
        let c3 = c2.DataNodes.[0] :?> YANGContainer
        Assert.Equal(2, c3.DataNodes.Count)
        Assert.Equal("first", c3.DataNodes.[0].Name.Name)
        Assert.Equal("second", c3.DataNodes.[1].Name.Name)

    [<Fact>]
    let ``Augments at top level require absolute node path`` () =
        let m =
            Utils.CreateModule """
                augment some-node {
                    leaf second { type int64; }
                }
            """
        match m with
        | Error [ AbsoluteNodePathRequired _ ] -> ()
        | _ -> failwithf "Should not have been parsed. Result: %A." m

    [<Fact>]
    let ``Errors on non-existing target`` () =
        let m =
            Utils.CreateModule """
                augment /no-node {
                    leaf l { type int64; }
                }
            """
        match m with
        | Error [ CannotResolvePath _ ] -> ()
        | _ -> failwithf "Should not have been parsed. Result: %A." m

    [<Fact>]
    let ``Augments can add nodes to other modules' nodes and preserve their original namespace`` () =
        
        // Create the module that will be the target of the augment
        let libRef = ref Unchecked.defaultof<YANGModule>
        let opts = YANGParserOptions(ResolveImport = fun _ _ -> Ok (!libRef))
        libRef :=
            YANGParser.ParseModule("""
                module my-lib {
                    namespace "http://example.com/my-lib";
                    prefix "lib";

                    container my-container {
                        leaf l1 {
                            type int32;
                        }
                    }
                }
            """, opts)
            |> Result.okOrThrow (string >> failwith)

        // Create the module that applies the augment
        let m =
            YANGParser.ParseModule("""
                module my-module {
                    namespace "http://example.com/my-module";
                    prefix "m";

                    import my-lib {
                        prefix "lib";
                    }

                    augment /lib:my-container {
                        leaf l2 {
                            type int64;
                        }
                    }
                }
            """, opts)
            |> Result.okOrThrow (string >> failwith)

        // Check that the module `m` has no data nodes, while `my-container` inside `lib` has a new leaf
        Assert.Equal(0, m.DataNodes.Count)
        Assert.Equal(1, (!libRef).DataNodes.Count)
        let c = (!libRef).DataNodes.[0] :?> YANGContainer
        Assert.Equal(2, c.DataNodes.Count)
        Assert.Equal("l1", c.DataNodes.[0].Name.Name)
        Assert.Equal("http://example.com/my-lib", c.DataNodes.[0].Name.Namespace.Uri.AbsolutePath)
        Assert.Equal("l2", c.DataNodes.[1].Name.Name)
        Assert.Equal("http://example.com/my-module", c.DataNodes.[1].Name.Namespace.Uri.AbsolutePath)

    [<Fact>]
    let ``Augments cannot add duplicate nodes`` () =
        let m =
            Utils.CreateModule """
                container cont {
                    leaf existing { type int32; }
                }

                augment /cont {
                    leaf existing { type int64; }
                }
            """
        match m with
        | Error [ AlreadyUsedName _ ] -> ()
        | _ -> failwithf "Should not have been parsed. Result: %A." m

    [<Fact>]
    let ``Augments can add duplicate nodes if they have different namespaces`` () =

        // Create the module that will be the target of the augment
        let libRef = ref Unchecked.defaultof<YANGModule>
        let opts = YANGParserOptions(ResolveImport = fun _ _ -> Ok (!libRef))
        libRef :=
            YANGParser.ParseModule("""
                module my-lib {
                    namespace "http://example.com/my-lib";
                    prefix "lib";

                    container my-container {
                        leaf existing-name {
                            type int32;
                        }
                    }
                }
            """, opts)
            |> Result.okOrThrow (string >> failwith)

        // Create the module that applies the augment
        let m =
            YANGParser.ParseModule("""
                module my-module {
                    namespace "http://example.com/my-module";
                    prefix "m";

                    import my-lib {
                        prefix "lib";
                    }

                    augment /lib:my-container {
                        leaf existing-name {
                            type int64;
                        }
                    }
                }
            """, opts)
            |> Result.okOrThrow (string >> invalidOp)

        // Check that the module `m` has no data nodes, while `my-container` inside `lib` has a new leaf
        Assert.Equal(0, m.DataNodes.Count)
        Assert.Equal(1, (!libRef).DataNodes.Count)
        let c = (!libRef).DataNodes.[0] :?> YANGContainer
        Assert.Equal(2, c.DataNodes.Count)
        Assert.Equal("existing-name", c.DataNodes.[0].Name.Name)
        Assert.Equal("http://example.com/my-lib", c.DataNodes.[0].Name.Namespace.Uri.AbsolutePath)
        Assert.Equal("existing-name", c.DataNodes.[1].Name.Name)
        Assert.Equal("http://example.com/my-module", c.DataNodes.[1].Name.Namespace.Uri.AbsolutePath)