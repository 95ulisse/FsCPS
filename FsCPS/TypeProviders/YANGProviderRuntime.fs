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
        dataReaders.Add(typeof<byte[]>, id >> box)
        dataReaders.Add(typeof<string>, Encoding.UTF8.GetString >> box)
        dataReaders.Add(typeof<double>, (fun arr -> BitConverter.Int64BitsToDouble(BitConverter.ToInt64(arr, 0)) |> box))
    

    [<EditorBrowsableAttribute(EditorBrowsableState.Never)>]
    [<CompilerMessageAttribute("This method is intended for use in generated code only.", 10001, IsHidden=true, IsError=false)>]
    let readAttribute<'a> (path: CPSPath) (obj: CPSObject) : 'a option =
        obj.GetAttribute(path)
        |> Option.map (fun attr ->
            dataReaders.[typeof<'a>](attr.Value) :?> 'a
        )