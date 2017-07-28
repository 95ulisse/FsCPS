namespace FsCPS.TypeProviders.Runtime

#nowarn "0686"
#nowarn "10001"

open System
open System.ComponentModel
open System.Collections.Generic
open System.Text
open FsCPS
open FsCPS.Native
open FsCPS.TypeProviders


[<EditorBrowsableAttribute(EditorBrowsableState.Never)>]
[<CompilerMessageAttribute("This type is intended for use in generated code only.", 10001, IsHidden=true, IsError=false)>]
type AttributePathSegment =
| Access of string
| Indexer


/// Internals of the runtime for the YANGProvider.
[<EditorBrowsableAttribute(EditorBrowsableState.Never)>]
[<CompilerMessageAttribute("This type is intended for use in generated code only.", 10001, IsHidden=true, IsError=false)>]
[<RequireQualifiedAccess>]
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


    let rec private listItem i lst =
        match lst with
        | [] -> None
        | _ when i < 0 -> None
        | x :: _ when i = 0 -> Some x
        | x :: xs -> listItem (i - 1) xs
    

    let private readPath segments indices (obj: CPSObject) =
        
        let rec f segments indices attr =
            match segments, indices with

            // Base case            
            | [], [] ->
                Some attr

            // Unbalanced indices
            | [], _ :: _ ->
                None

            // Access to a fixed attribute ID.
            // This is used to access properties inside containers.
            | Access attrPath :: remainingSegs, _ ->
                let attrId = attrPath
                             |> NativeMethods.AttrIdFromPath
                             |> Result.okOrThrow failwith
                             |> CPSAttributeID
                match attr with
                | Container (_, map) ->
                    map
                    |> Map.tryFind attrId
                    |> Option.bind (f remainingSegs indices)
                | _ ->
                    None

            // Parametrized indexer.
            // This is used to access a specific container in a list.
            | Indexer :: remainingSegs, index :: remainingIndices ->
                match attr with
                | List (_, lst) ->
                    lst
                    |> listItem index
                    |> Option.map (fun map -> Container ((CPSAttributeID 0UL), map))
                    |> Option.bind (f remainingSegs remainingIndices)
                | _ ->
                    None

            // Parametrized indexer, but missing index
            | Indexer :: _, [] ->
                None
        
        f segments indices (Container ((CPSAttributeID 0UL), obj.Attributes))

       

    /// Registers a new type along with its reader and writer.
    let registerType<'a> (reader: byte[] -> obj) (writer: obj -> byte[]) =
        if transformers.ContainsKey(typeof<'a>) then
            invalidOp (sprintf "Type %s has already been registered." typeof<'a>.Name)
        transformers.Add(typeof<'a>, (reader, writer))
        
            
    [<EditorBrowsableAttribute(EditorBrowsableState.Never)>]
    [<CompilerMessageAttribute("This method is intended for use in generated code only.", 10001, IsHidden=true, IsError=false)>]
    let readLeaf<'a> path indices obj =
        match transformers.TryGetValue(typeof<'a>) with
        | (true, (reader, _)) ->
            readPath path indices obj
            |> (Option.map <| function
                              | Leaf (_, v) -> v |> reader |> unbox<'a>
                              | _ -> invalidOp "Attribute read is not a leaf.")
        | _ ->
            invalidArg "'a" (sprintf "Type %s has not been registered. Please, use YANGProviderRuntime.registerType to register a new type." typeof<'a>.Name)
            
            
    [<EditorBrowsableAttribute(EditorBrowsableState.Never)>]
    [<CompilerMessageAttribute("This method is intended for use in generated code only.", 10001, IsHidden=true, IsError=false)>]
    let writeLeaf<'a> path indices (attrVal: 'a option) obj =
        match transformers.TryGetValue(typeof<'a>) with
        | (true, (_, writer)) ->
            raise <| NotImplementedException() |> ignore
        | _ ->
            invalidArg "'a" (sprintf "Type %s has not been registered. Please, use YANGProviderRuntime.registerType to register a new type." typeof<'a>.Name)


    [<EditorBrowsableAttribute(EditorBrowsableState.Never)>]
    [<CompilerMessageAttribute("This method is intended for use in generated code only.", 10001, IsHidden=true, IsError=false)>]
    let readLeafList<'a> path indices obj =
        match transformers.TryGetValue(typeof<'a>) with
        | (true, (reader, _)) ->
            readPath path indices obj
            |> (Option.map <| function
                              | LeafList (_, v) -> v |> List.map (reader >> unbox<'a>)
                              | _ -> invalidOp "Attribute read is not a leaf-list.")
        | _ ->
            invalidArg "'a" (sprintf "Type %s has not been registered. Please, use YANGProviderRuntime.registerType to register a new type." typeof<'a>.Name)
            
            
    [<EditorBrowsableAttribute(EditorBrowsableState.Never)>]
    [<CompilerMessageAttribute("This method is intended for use in generated code only.", 10001, IsHidden=true, IsError=false)>]
    let writeLeafList<'a> path indices (attrVal: 'a list option) obj =
        match transformers.TryGetValue(typeof<'a>) with
        | (true, (_, writer)) ->
            raise <| NotImplementedException() |> ignore
        | _ ->
            invalidArg "'a" (sprintf "Type %s has not been registered. Please, use YANGProviderRuntime.registerType to register a new type." typeof<'a>.Name)


    // ----------------------------------------------------------------------------------------
    // ----------------------------------------------------------------------------------------


    [<EditorBrowsableAttribute(EditorBrowsableState.Never)>]
    [<CompilerMessageAttribute("This type is intended for use in generated code only.", 10001, IsHidden=true, IsError=false)>]
    let validateObject (obj: CPSObject) (key: string) =
        if obj.Key.Key = key then
            Ok (obj, List.empty<AttributePathSegment>, List.empty<int>)
        else
            Error MismatchingKey