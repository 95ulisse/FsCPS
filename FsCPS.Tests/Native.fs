namespace FsCPS.Tests

open Xunit
open FsCheck
open FsCheck.Xunit
open FsCPS
open FsCPS.Native
open FsCPS.Tests.Utils


// String representation of a native key
type NativeKeyString = NativeKeyString of string


type Arbitraries() =
    static member NativeKey() =
        [
            "1.34.";
            "1.34.2228241.2228246.2228236.";
            "1.34.2228241.2228246.2228236.2228240.2228228.";
            "1.34.2228241.2228246.2228236.2228240.2228228.2228230.";
            "1.204.13369376.13369371.13369372.";
            "1.204.13369377.13369370.13369345."
        ]
        |> Gen.elements
        |> Gen.map NativeKeyString
        |> Arb.fromGen
    static member CPSPath() =
        [
            //"base-ip";
            //"base-ip/ipv4";
            //"base-ip/ipv4/address";
            "base-ip/ipv4/address/ip";
            "base-ip/ipv4/ifindex"
        ]
        |> Gen.elements
        |> Gen.map CPSPath
        |> Arb.fromGen
    static member CPSPathList() =
        Arb.generate<CPSPath>
        |> Gen.listOf
        |> Gen.map List.distinct
        |> Arb.fromGen
        
[<Properties(Arbitrary = [| typeof<Arbitraries> |])>]
module Native =

    [<Property>]
    let internal ``Key parsing reports failure for invalid keys`` (CPSPath path) =
        match NativeMethods.ParseKey path with
        | Error _ -> true
        | _ -> false

    [<Property>]
    let internal ``CPSKey constructor throws if an invalid key is passed`` (CPSPath path) =
        try
            CPSKey path |> ignore
            false
        with
        | _ -> true
    
    [<Property>]
    let internal ``Keys can be converted from string to binary and viceversa`` (NativeKeyString str) =
        let str2 =
            NativeMethods.ParseKey str
            |>> (fun k ->
                let res = NativeMethods.PrintKey(k.Address)
                k.Dispose()
                res
            )
            |> Result.okOrThrow invalidOp
        str = str2

    [<Property>]
    let internal ``Object keys can be set and retrived`` (NativeKeyString str) =
        NativeMethods.CreateObject()
        >>= NativeMethods.SetObjectKey str
        >>= NativeMethods.GetObjectKey
        |>> NativeMethods.PrintKey
        |>> (fun str2 -> str = str2)
        |> Result.okOrThrow invalidOp
    
    [<Property>]
    let internal ``Object lists can be populated and iterated`` (keys: NativeKeyString list) =

        // Creates a new object for each key
        let objects =
            keys
            |> List.map (fun (NativeKeyString str) ->
                NativeMethods.CreateObject()
                >>= NativeMethods.SetObjectKey str
                |> Result.okOrThrow invalidOp
            )

        // Folds the objects into a list
        let list =
            objects
            |> Result.foldSequence
                NativeMethods.AppendObjectToList
                (NativeMethods.CreateObjectList() |> Result.okOrThrow invalidOp)
            |> Result.okOrThrow invalidOp

        // Iterates back the list and makes sure that the objects are iterated in the same original order
        let result =
            list
            |> NativeMethods.IterateObjectList
            |> Seq.zip objects
            |> Seq.forall (fun (obj, expectedObj) -> obj = expectedObj)

        // Cleanup
        NativeMethods.DestroyObjectList true list
        |> Result.okOrThrow invalidOp

        result
        
    [<Property>]
    let ``Object attributes can be iterated upon`` (NativeKeyString key) (paths: CPSPath list) =
        
        // Creates a new object with the given attributes
        let managedObj = CPSObject(CPSKey(key))
        paths
        |> List.iter (fun path -> managedObj.SetAttribute(path, [| 1uy; 2uy; 3uy; 4uy |]))

        // Convert the object to a native one and try yo iterate the attributes
        managedObj.ToNativeObject()
        |>> (fun nativeObject ->
            NativeMethods.BeginAttributeIterator(nativeObject).Iterate()
            |> Seq.fold (fun (res, count) it ->
                let attrId = NativeMethods.AttrIdFromAttr(it.attr)
                if not res then
                    (false, 0)
                else
                    NativeMethods.AttrIdToPath attrId
                    |>> CPSPath
                    >>= (fun path ->
                        match managedObj.GetAttribute(path) with
                        | None -> Ok(false, 0)
                        | Some(Leaf(_, attr)) ->
                            NativeMethods.GetAttribute nativeObject attrId
                            |>> (fun attrValue -> (attrValue = attr, count + 1))
                        | _ ->
                            invalidOp "Testing works only for leafs at the moment"
                    )
                    |> function
                        | Ok x -> x
                        | Error _ -> (false, 0)
            ) (true, 0)
            |> (fun (res, count) ->
                Assert.True(res, "Attributes do not correspond.")
                Assert.Equal(managedObj.Attributes.Count, count)
            )
            nativeObject
        )
        |>> NativeMethods.DestroyObject

        // Raise an exception in case of error
        |> Result.okOrThrow invalidOp

    [<Property>]
    let  ``Reconstructing a managed object from a native one preserves attributes`` (NativeKeyString key) (paths: CPSPath list) =
        
        // Creates a new object with the given attributes
        let obj = CPSObject(CPSKey(key))
        paths
        |> List.iter (fun path -> obj.SetAttribute(path, [| 1uy; 2uy; 3uy; 4uy |]))

        // Convert the object to native and back to managed
        let obj2 =
            obj.ToNativeObject()
            >>= (fun nativeObj ->
                CPSObject.FromNativeObject nativeObj
                |> Result.tee (fun _ -> Ok(NativeMethods.DestroyObject nativeObj))
            )
            |> Result.okOrThrow invalidOp

        // Check that the objects have the same data
        AssertCPSObjectEquals obj obj2