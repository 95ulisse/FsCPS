namespace FsCPS.TypeProviders.Runtime

#nowarn "0686"

open System
open System.ComponentModel
open System.Collections.Generic
open System.Text
open FsCPS

/// Internals of the runtime for the YANGProvider.
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
        let inline add (f: ^a -> byte[]) = dataWriters.Add(typeof< ^a >, unbox< ^a > >> f)
        add<bool> BitConverter.GetBytes
        add<int8> (fun x -> [| byte x |])
        add<int16> BitConverter.GetBytes
        add<int32> BitConverter.GetBytes
        add<int64> BitConverter.GetBytes
        add<uint8> (fun x -> [| byte x |])
        add<uint16> BitConverter.GetBytes
        add<uint32> BitConverter.GetBytes
        add<uint64> BitConverter.GetBytes
        add<byte[]> id
        add<string> Encoding.UTF8.GetBytes
        add<double> (BitConverter.DoubleToInt64Bits >> BitConverter.GetBytes)
        

    /// Registers a new type along with its reader and writer.
    let registerType<'a> (reader: byte[] -> obj) (writer: obj -> byte[]) =
        if dataReaders.ContainsKey(typeof<'a>) then
            invalidOp (sprintf "Type %s has already been registered." typeof<'a>.Name)
        dataReaders.Add(typeof<'a>, reader)
        dataWriters.Add(typeof<'a>, writer)
        
            
    [<EditorBrowsableAttribute(EditorBrowsableState.Never)>]
    [<CompilerMessageAttribute("This method is intended for use in generated code only.", 10001, IsHidden=true, IsError=false)>]
    let readAttribute<'a> (path: CPSPath) (obj: CPSObject) : 'a option =
        match dataReaders.TryGetValue(typeof<'a>) with
        | (true, reader) ->
            obj.GetAttribute(path)
            |> Option.map (fun attr -> reader attr.Value :?> 'a)
        | _ ->
            invalidArg "'a" (sprintf "Type %s has not been registered. Please, use YANGProviderRuntime.registerType to register a new type." typeof<'a>.Name)
            
            
    [<EditorBrowsableAttribute(EditorBrowsableState.Never)>]
    [<CompilerMessageAttribute("This method is intended for use in generated code only.", 10001, IsHidden=true, IsError=false)>]
    let writeAttribute<'a> (path: CPSPath) (attrVal: 'a option) (obj: CPSObject) =
        match dataWriters.TryGetValue(typeof<'a>) with
        | (true, writer) ->
            match attrVal with
            | Some v -> obj.SetAttribute(path, writer (box v))
            | None -> obj.RemoveAttribute(path) |> ignore
        | _ ->
            invalidArg "'a" (sprintf "Type %s has not been registered. Please, use YANGProviderRuntime.registerType to register a new type." typeof<'a>.Name)