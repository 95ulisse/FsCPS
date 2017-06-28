namespace FsCPS.TypeProviders.Runtime

open System
open System.ComponentModel
open System.Collections.Generic
open System.Text
open FsCPS

[<EditorBrowsableAttribute(EditorBrowsableState.Never)>]
[<CompilerMessageAttribute("This module is intended for use in generated code only.", 10001, IsHidden=true, IsError=false)>]
module YANGProviderRuntime =

    let private dataReaders = Dictionary<Type, byte[] -> obj>()
    let private dataWriters = Dictionary<Type, obj -> byte[]>()

    // Common data readers
    do
        let wrap f x = box (f(x, 0))
        dataReaders.Add(typeof<bool>, wrap BitConverter.ToBoolean)
        dataReaders.Add(typeof<int8>, (fun arr -> box (int8 arr.[0])))
        dataReaders.Add(typeof<int16>, wrap BitConverter.ToInt16)
        dataReaders.Add(typeof<int32>, wrap BitConverter.ToInt32)
        dataReaders.Add(typeof<int64>, wrap BitConverter.ToInt64)
        dataReaders.Add(typeof<uint8>, (fun arr -> box (uint8 arr.[0])))
        dataReaders.Add(typeof<uint16>, wrap BitConverter.ToUInt16)
        dataReaders.Add(typeof<uint32>, wrap BitConverter.ToUInt32)
        dataReaders.Add(typeof<uint64>, wrap BitConverter.ToUInt64)
        dataReaders.Add(typeof<byte[]>, box)
        dataReaders.Add(typeof<string>, Encoding.UTF8.GetString >> box)
        dataReaders.Add(typeof<double>, (fun arr -> BitConverter.Int64BitsToDouble(BitConverter.ToInt64(arr, 0)) |> box))
        
    // Common data writers
    do
        let inline w (f: ^a -> byte[]) = dataWriters.Add(typeof< ^a >, unbox< ^a > >> f)
        w<bool> BitConverter.GetBytes
        w<int8> (fun x -> [| byte x |])
        w<int16> BitConverter.GetBytes
        w<int32> BitConverter.GetBytes
        w<int64> BitConverter.GetBytes
        w<uint8> (fun x -> [| byte x |])
        w<uint16> BitConverter.GetBytes
        w<uint32> BitConverter.GetBytes
        w<uint64> BitConverter.GetBytes
        w<byte[]> id
        w<string> Encoding.UTF8.GetBytes
        w<double> (BitConverter.DoubleToInt64Bits >> BitConverter.GetBytes)
        

    [<EditorBrowsableAttribute(EditorBrowsableState.Never)>]
    [<CompilerMessageAttribute("This method is intended for use in generated code only.", 10001, IsHidden=true, IsError=false)>]
    let readAttribute<'a> (path: CPSPath) (obj: CPSObject) : 'a option =
        obj.GetAttribute(path)
        |> Option.map (fun attr ->
            dataReaders.[typeof<'a>](attr.Value) :?> 'a
        )
        
    [<EditorBrowsableAttribute(EditorBrowsableState.Never)>]
    [<CompilerMessageAttribute("This method is intended for use in generated code only.", 10001, IsHidden=true, IsError=false)>]
    let writeAttribute<'a> (path: CPSPath) (attrVal: 'a option) (obj: CPSObject) =
        match attrVal with
        | Some v ->
            let arr = dataWriters.[typeof<'a>](box v)
            obj.SetAttribute(path, arr)
        | None ->
            obj.RemoveAttribute(path) |> ignore