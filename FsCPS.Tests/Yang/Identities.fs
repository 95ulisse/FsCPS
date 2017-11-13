namespace FsCPS.Tests.Yang

open System
open Xunit
open FsCPS
open FsCPS.Tests
open FsCPS.Yang

module Identities =
    
    [<Fact>]
    let ``Identities are exported in the module they are defined`` () =
        let m =
            Utils.CreateModule """
                identity my-identity;
            """
            |> Result.okOrThrow (string >> invalidOp)
        Assert.Equal(1, m.ExportedIdentities.Count)
        let id = m.ExportedIdentities.Values |> Seq.head
        Assert.Equal("my-identity", id.Name.Name)


    [<Fact>]
    let ``Identities can be used in typedefs`` () =
        let m =
            Utils.CreateModule """
                identity my-identity;

                typedef my-type {
                    type identityref {
                        base my-identity;
                    }
                }
            """
            |> Result.okOrThrow (string >> invalidOp)
        Assert.Equal(1, m.ExportedTypes.Count)
        let t = m.ExportedTypes.Values |> Seq.head
        match t.BaseType.Value.ResolvedType with
        | IdentityRef idRef ->
            Assert.Equal("my-identity", idRef.ResolvedIdentity.Name.Name)
        | _ ->
            invalidOp "Invalid base type"


    [<Fact>]
    let ``Derived identities can be used in typedefs`` () =
        let m =
            Utils.CreateModule """
                identity my-identity;
                identity my-derived-identity {
                    base my-identity;
                }

                typedef my-type {
                    type identityref {
                        base my-derived-identity;
                    }
                }
            """
            |> Result.okOrThrow (string >> invalidOp)
        Assert.Equal(1, m.ExportedTypes.Count)
        let t = m.ExportedTypes.Values |> Seq.head
        match t.BaseType.Value.ResolvedType with
        | IdentityRef idRef ->
            Assert.Equal("my-derived-identity", idRef.ResolvedIdentity.Name.Name)
            Assert.Equal("my-identity", idRef.ResolvedIdentity.Base.Value.ResolvedIdentity.Name.Name)
        | _ ->
            invalidOp "Invalid base type"


    [<Fact>]
    let ``Identities from other modules can be used`` () =

        // Create the module that will be the target of the augment
        let libRef = ref Unchecked.defaultof<YANGModule>
        let opts = YANGParserOptions(ResolveImport = fun _ _ -> Ok (!libRef))
        libRef :=
            YANGParser.ParseModule("""
                module my-lib {
                    namespace "http://example.com/my-lib";
                    prefix "lib";

                    identity my-identity;
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

                    typedef my-type {
                        type identityref {
                            base lib:my-identity;
                        }
                    }
                }
            """, opts)
            |> Result.okOrThrow (string >> invalidOp)

        let t = m.ExportedTypes.Values |> Seq.head
        match t.BaseType.Value.ResolvedType with
        | IdentityRef idRef ->
            let id = idRef.ResolvedIdentity
            Assert.Equal("http://example.com/my-lib", id.Name.Namespace.Uri.AbsoluteUri)
            Assert.Equal("my-identity", id.Name.Name)
        | _ ->
            invalidOp "Invalid base type"


    [<Fact>]
    let ``Identities can derive from identities in other modules`` () =

        // Create the module that will be the target of the augment
        let libRef = ref Unchecked.defaultof<YANGModule>
        let opts = YANGParserOptions(ResolveImport = fun _ _ -> Ok (!libRef))
        libRef :=
            YANGParser.ParseModule("""
                module my-lib {
                    namespace "http://example.com/my-lib";
                    prefix "lib";

                    identity my-identity;
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

                    identity my-derived-identity {
                        base lib:my-identity;
                    }

                    typedef my-type {
                        type identityref {
                            base my-derived-identity;
                        }
                    }
                }
            """, opts)
            |> Result.okOrThrow (string >> invalidOp)

        let t = m.ExportedTypes.Values |> Seq.head
        match t.BaseType.Value.ResolvedType with
        | IdentityRef idRef ->
            let id = idRef.ResolvedIdentity
            Assert.Equal("http://example.com/my-module", id.Name.Namespace.Uri.AbsoluteUri)
            Assert.Equal("my-derived-identity", id.Name.Name)
            let baseId = id.Base.Value.ResolvedIdentity
            Assert.Equal("http://example.com/my-lib", baseId.Name.Namespace.Uri.AbsoluteUri)
            Assert.Equal("my-identity", baseId.Name.Name)
        | _ ->
            invalidOp "Invalid base type"