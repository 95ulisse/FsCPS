namespace FsCPS.TypeProviders.Runtime

#nowarn "0686"

open System
open System.ComponentModel
open System.Collections.Generic
open System.Text
open FsCPS

/// Internals of the runtime for the YANGProvider.
module YANGProviderRuntime =

    let private transformers = Dictionary<Type, (byte[] -> obj) * (obj -> byte[])>()

    // Transformers for common types
    do
        let inline wrapr f x = f(x, 0) |> box
        let inline wrapw (f: ^a -> byte[]) = unbox< ^a > >> f
        let inline add (reader: byte[] * int -> ^a) (writer: ^a -> byte[]) =
            transformers.Add(typeof< ^a >, (wrapr reader, wrapw writer))

        add<bool> BitConverter.ToBoolean BitConverter.GetBytes
        add<int8> (fun (arr, _) -> int8 arr.[0]) (fun x -> [| byte x |])
        add<int16> BitConverter.ToInt16 BitConverter.GetBytes
        add<int32> BitConverter.ToInt32 BitConverter.GetBytes
        add<int64> BitConverter.ToInt64 BitConverter.GetBytes
        add<uint8> (fun (arr, _) -> uint8 arr.[0]) (fun x -> [| byte x |])
        add<uint16> BitConverter.ToUInt16 BitConverter.GetBytes
        add<uint32> BitConverter.ToUInt32 BitConverter.GetBytes
        add<uint64> BitConverter.ToUInt64 BitConverter.GetBytes
        add<byte[]> (fun (arr, _) -> arr) id
        add<string> (fun (arr, _) -> Encoding.UTF8.GetString arr) Encoding.UTF8.GetBytes
        add<double> (fun (arr, _) -> BitConverter.Int64BitsToDouble(BitConverter.ToInt64(arr, 0)))
                    (BitConverter.DoubleToInt64Bits >> BitConverter.GetBytes)
       

    /// Registers a new type along with its reader and writer.
    let registerType<'a> (reader: byte[] -> obj) (writer: obj -> byte[]) =
        if transformers.ContainsKey(typeof<'a>) then
            invalidOp (sprintf "Type %s has already been registered." typeof<'a>.Name)
        transformers.Add(typeof<'a>, (reader, writer))
        
            
    [<EditorBrowsableAttribute(EditorBrowsableState.Never)>]
    [<CompilerMessageAttribute("This method is intended for use in generated code only.", 10001, IsHidden=true, IsError=false)>]
    let readLeaf<'a> (path: CPSPath) (obj: CPSObject) : 'a option =
        match transformers.TryGetValue(typeof<'a>) with
        | (true, (reader, _)) ->
            obj.GetAttribute(path)
            |> Option.map (fun attr -> reader attr :?> 'a)
        | _ ->
            invalidArg "'a" (sprintf "Type %s has not been registered. Please, use YANGProviderRuntime.registerType to register a new type." typeof<'a>.Name)
            
            
    [<EditorBrowsableAttribute(EditorBrowsableState.Never)>]
    [<CompilerMessageAttribute("This method is intended for use in generated code only.", 10001, IsHidden=true, IsError=false)>]
    let writeLeaf<'a> (path: CPSPath) (attrVal: 'a option) (obj: CPSObject) =
        match transformers.TryGetValue(typeof<'a>) with
        | (true, (_, writer)) ->
            match attrVal with
            | Some v -> obj.SetAttribute(path, writer (box v))
            | None -> obj.RemoveAttribute(path) |> ignore
        | _ ->
            invalidArg "'a" (sprintf "Type %s has not been registered. Please, use YANGProviderRuntime.registerType to register a new type." typeof<'a>.Name)


    [<EditorBrowsableAttribute(EditorBrowsableState.Never)>]
    [<CompilerMessageAttribute("This method is intended for use in generated code only.", 10001, IsHidden=true, IsError=false)>]
    let readLeafList<'a> (path: CPSPath) (obj: CPSObject) : 'a list option =
        match transformers.TryGetValue(typeof<'a>) with
        | (true, (reader, _)) ->
            obj.GetAttribute(path, CPSAttributeClass.LeafList)
            |> Option.bind (function
                            | LeafList (_, lst) -> lst |> List.map (reader >> unbox<'a>) |> Some
                            | _ -> None)
        | _ ->
            invalidArg "'a" (sprintf "Type %s has not been registered. Please, use YANGProviderRuntime.registerType to register a new type." typeof<'a>.Name)
            
            
    [<EditorBrowsableAttribute(EditorBrowsableState.Never)>]
    [<CompilerMessageAttribute("This method is intended for use in generated code only.", 10001, IsHidden=true, IsError=false)>]
    let writeLeafList<'a> (path: CPSPath) (attrVal: 'a list option) (obj: CPSObject) =
        match transformers.TryGetValue(typeof<'a>) with
        | (true, (_, writer)) ->
            match attrVal with
            | Some lst -> obj.SetAttribute(path, lst |> List.map writer)
            | None -> obj.RemoveAttribute(path) |> ignore
        | _ ->
            invalidArg "'a" (sprintf "Type %s has not been registered. Please, use YANGProviderRuntime.registerType to register a new type." typeof<'a>.Name)
