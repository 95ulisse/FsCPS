namespace FsCPS.Tests.Yang

open System
open Xunit
open FsCPS
open FsCPS.Tests
open FsCPS.Yang

module Imports =
    
    [<Fact>]
    let ``The builtin modules are always available`` () =
        let builtins = [
            "ietf-yang-types";
            "ietf-inet-types"
        ]
        builtins |> Seq.iter (fun builtinModule ->
            Utils.CreateModule("""
                import """ + builtinModule + """ {
                    prefix builtin;
                }
            """)
            |> Utils.AssertOk
            |> ignore
        )

    [<Fact>]
    let ``Types from the imported modules can be referenced`` () =
        let m =
            Utils.CreateModule("""
                import ietf-yang-types {
                    prefix yang;
                }
                typedef derived-type {
                    type yang:counter32;
                }
            """)
            |> Utils.AssertOk
        let t = m.ExportedTypes.[{ Name = "derived-type"; Namespace = m.Namespace }]
        Assert.NotNull(t)
        Assert.True(t.BaseType.IsSome)
        let baseType = t.BaseType.Value.ResolvedType
        Assert.Equal(baseType.Name.Name, "counter32")
        Assert.Equal(baseType.Name.Namespace.Uri.AbsoluteUri, "urn:ietf:params:xml:ns:yang:ietf-yang-types")