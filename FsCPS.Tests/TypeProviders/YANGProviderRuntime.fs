namespace FsCPS.Tests.TypeProviders

#nowarn "10001"

open System
open Xunit
open FsCheck
open FsCheck.Xunit
open FsCPS
open FsCPS.TypeProviders.Runtime
open FsCPS.TypeProviders.Runtime.YANGProviderRuntime


type Arbitraries() =
    static member CPSAttributeID() =
        Arb.generate<uint64>
        |> Gen.filter ((<>) 0UL)
        |> Gen.map CPSAttributeID
        |> Arb.fromGen


[<Properties(Arbitrary = [| typeof<Arbitraries> |])>]
module YANGProviderRuntime =

    [<Fact>]
    let ``Write fails with empty path`` () =
        let o = CPSObject(CPSKey "1.34.")
        Assert.Throws<ArgumentException>(fun () -> YANGProviderRuntime.writeLeaf<int> [] [] None o) |> ignore
        Assert.Throws<ArgumentException>(fun () -> YANGProviderRuntime.writeLeafList<int> [] [] None o) |> ignore
    
    [<Property>]
    let ``Can write a root leaf`` attrId =
        let o = CPSObject(CPSKey "1.34.")
        o |> YANGProviderRuntime.writeLeaf<int> [ Access attrId ] [] (Some 1)
        match o.GetAttribute(attrId) with
        | Some (Leaf (attrId, v)) -> v = [| 1uy; 0uy; 0uy; 0uy |]
        | _ -> failwith "Read failure."

    [<Property>]
    let ``Can write a root leaf-list`` attrId =
        let o = CPSObject(CPSKey "1.34.")
        o |> YANGProviderRuntime.writeLeafList<int> [ Access attrId ] [] (Some [ 1 ])
        match o.GetAttribute(attrId) with
        | Some (LeafList (attrId, v)) -> v = [ [| 1uy; 0uy; 0uy; 0uy |] ]
        | _ -> failwith "Read failure."

    [<Property>]
    let ``Write creates all intermediate containers if not already existing`` attrs =
        (List.length attrs) >= 2 ==> lazy (
            let o = CPSObject(CPSKey "1.34.")
            o |> YANGProviderRuntime.writeLeaf<int> (attrs |> List.map Access) [] (Some 1)
            attrs |> foldi (fun attr i aid ->
                match attr with
                | Container (_, inner) -> Map.find aid inner
                | Leaf (_, [| 1uy; 0uy; 0uy; 0uy |]) when i = attrs.Length - 1 -> attr
                | _ -> failwith "Missing intermediate container."
            ) (Container (CPSAttributeID 0UL, o.Attributes))
            |> ignore
        )

    [<Property>]
    let ``Write preserves intermediate containers if already existing`` attrs =
        (List.length attrs) >= 2 ==> lazy (
            let o = CPSObject(CPSKey "1.34.")

            // Write the attribute (this creates intermediate containers)
            o |> YANGProviderRuntime.writeLeaf<int> (attrs |> List.map Access) [] (Some 1)

            // Add a leaf for each container
            attrs
            |> List.scan (fun state aid ->
                Access aid :: state
            ) []
            |> List.map List.rev
            |> List.map (fun x -> x @ [ Access (CPSAttributeID 0UL) ])
            |> List.filter (fun x -> x.Length <= attrs.Length)
            |> List.iter (fun path ->
                o |> YANGProviderRuntime.writeLeaf<int> path [] (Some 0)
            )

            // Change the original deep leaf
            o |> YANGProviderRuntime.writeLeaf<int> (attrs |> List.map Access) [] (Some 2)

            // Check that all the containers and the intermediate leafs exist
            attrs |> foldi (fun attr i aid ->
                match attr with
                | Container (_, inner) ->
                    Assert.True((Map.tryFind (CPSAttributeID 0UL) inner).IsSome)
                    Map.find aid inner
                | Leaf (_, [| 2uy; 0uy; 0uy; 0uy |]) when i = attrs.Length - 1 -> attr
                | _ -> failwith "Missing intermediate container."
            ) (Container (CPSAttributeID 0UL, o.Attributes))
            |> ignore
        )

    [<Property>]
    let ``Write a list item allocates space in the list`` attrs =
        (List.length attrs) >= 2 ==> lazy (
            let o = CPSObject(CPSKey "1.34.")

            let path =
                attrs
                |> List.indexed
                |> List.collect (fun (i, aid) -> if i = attrs.Length - 1 then [ Access aid ] else [ Access aid; Indexer ])
            
            let indices =
                List.init (attrs.Length - 1) id

            // Write the attribute (this creates intermediate lists)
            o |> YANGProviderRuntime.writeLeaf<int> path indices (Some 1)

            // Checks the validity of the lists
            attrs |> foldi (fun attr i aid ->
                match attr with
                | Container (_, inner) ->
                    match Map.find aid inner with
                    | List (_, lst) ->
                        Assert.Equal(i + 1, lst.Length)
                        Container(aid, lst.[i])
                    | Leaf (_, [| 1uy; 0uy; 0uy; 0uy |]) when i = attrs.Length - 1 -> attr
                    | _ -> failwith "Missing intermediate list."
                | _ -> failwith "Missing intermediate list."
            ) (Container (CPSAttributeID 0UL, o.Attributes))
            |> ignore
        )

    [<Fact>]
    let ``Read fails with empty path`` () =
        let o = CPSObject(CPSKey "1.34.")
        Assert.Throws<ArgumentException>(fun () -> YANGProviderRuntime.readLeaf<int> [] [] o |> ignore) |> ignore
        Assert.Throws<ArgumentException>(fun () -> YANGProviderRuntime.readLeafList<int> [] [] o |> ignore) |> ignore

    [<Property>]
    let ``Can read a root leaf`` attrId =
        let o = CPSObject(CPSKey "1.34.")
        let read () =
            YANGProviderRuntime.readLeaf<byte> [ Access attrId ] [] o

        Assert.True(read().IsNone)

        o.SetAttribute(Leaf(attrId, [| 1uy |]))
        read().Value = 1uy

    [<Property>]
    let ``Can read a root leaf-list`` attrId =
        let o = CPSObject(CPSKey "1.34.")
        let read () =
            YANGProviderRuntime.readLeafList<byte> [ Access attrId ] [] o

        Assert.True(read().IsNone)

        o.SetAttribute(LeafList(attrId, [ [| 1uy |] ]))
        read().Value = [ 1uy ]

    [<Property>]
    let ``Can read nested containers`` attrs =
        (List.length attrs) >= 2 ==> lazy (
            let o = CPSObject(CPSKey "1.34.")
            let path = attrs |> List.map Access
            o |> YANGProviderRuntime.writeLeaf<int> path [] (Some 1)

            (YANGProviderRuntime.readLeaf<int> path [] o).Value = 1
        )

    [<Property>]
    let ``Can read nested lists`` attrs =
        (List.length attrs) >= 2 ==> lazy (
            let o = CPSObject(CPSKey "1.34.")

            let path =
                attrs
                |> List.indexed
                |> List.collect (fun (i, aid) -> if i = attrs.Length - 1 then [ Access aid ] else [ Access aid; Indexer ])
            
            let indices =
                List.init (attrs.Length - 1) id

            o |> YANGProviderRuntime.writeLeaf<int> path indices (Some 1)

            (YANGProviderRuntime.readLeaf<int> path indices o).Value = 1
        )