namespace FsCPS.Tests

open Xunit
open FsCheck
open FsCheck.Xunit
open FsCPS
open FsCPS.Native


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

        
[<Properties(Arbitrary = [| typeof<Arbitraries> |])>]
module Native =
    
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
            |> foldResult
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