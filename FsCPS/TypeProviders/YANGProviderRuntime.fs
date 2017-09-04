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


/// Internals of the runtime for the YANGProvider.
[<EditorBrowsableAttribute(EditorBrowsableState.Never)>]
[<CompilerMessageAttribute("This type is intended for use in generated code only.", 10001, IsHidden=true, IsError=false)>]
module YANGProviderRuntime =

    [<EditorBrowsableAttribute(EditorBrowsableState.Never)>]
    [<CompilerMessageAttribute("This type is intended for use in generated code only.", 10001, IsHidden=true, IsError=false)>]
    type internal AttributePathSegment =
    | Access of CPSAttributeID
    | Indexer

    let private transformers = Dictionary<Type, (byte[] -> obj) * (obj -> byte[])>()

    // Transformers for common types
    do
        let inline wrapr f x = f(x, 0) |> box
        let inline wrapw (f: ^a -> byte[]) = unbox< ^a > >> f
        let inline add (reader: byte[] * int -> ^a) (writer: ^a -> byte[]) =
            transformers.Add(typeof< ^a >, (wrapr reader, wrapw writer))

        add<unit> (fun _ -> ()) (fun _ -> Array.empty<_>)
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

        if segments = [] then
            invalidArg "segments" "Invalid empty read path."
        
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
            | Access attrId :: remainingSegs, _ ->
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


    let private writePath segments indices (obj: CPSObject) (value: obj option) =
        
        let makeAttribute id (value: obj) =
            match value with
            | :? array<byte> as v -> Leaf(id, v)
            | :? list<array<byte>> as v -> LeafList(id, v)
            | _ -> invalidArg "value" "Unexpected value to write: provide a byte array to write a leaf, or a byte array list to write a leaf-list."

        let findOrCreateMap id map =
            match Map.tryFind id map with
            | Some (Container (_, x)) -> x
            | Some _ -> invalidArg "segments" "Invalid path access: expected Container."
            | None -> Map.empty<_, _>

        let findOrCreateList id i f map =

            // Get the list from the map, or use an empty one
            let lst =
                match Map.tryFind id map with
                | Some (List (_, lst)) -> lst
                | Some _ -> invalidArg "segments" "Invalid path access: expected List."
                | None -> []

            // Rebuild a list which is a clone of the extracted one, but with the i-th item modified
            if i < lst.Length then
                lst
                |> List.indexed
                |> List.fold (fun state (j, x) ->
                    if i = j then
                        (f x) :: state
                    else
                        x :: state
                ) []
            else
                lst @ List.init (i - lst.Length + 1) (fun j ->
                    if i = j then
                        f Map.empty<_, _>
                    else
                        Map.empty<_, _>
                )

        let rec f segments indices map =
            match segments, indices with

            // Base case
            | Access attrId :: [], [] ->
                match value with
                | Some v -> Map.add attrId (makeAttribute attrId v) map
                | None -> Map.remove attrId map

            // Access a list
            | Access listId :: Indexer :: remainingSegments, i :: remainingIndices when remainingSegments <> [] ->
                let innerList = findOrCreateList listId i (f remainingSegments remainingIndices) map
                Map.add listId (List (listId, innerList)) map

            // Access a container
            | Access attrId :: remainingSegments, _ when remainingSegments <> [] ->
                let innerMap = findOrCreateMap attrId map
                               |> f remainingSegments indices
                Map.add attrId (Container (attrId, innerMap)) map

            // Every other case is a mismatched path
            | _ ->
                invalidArg "segments" "Unbalanced or empty write path."

        obj.Attributes <- f segments indices obj.Attributes

       

    /// Registers a new type along with its reader and writer.
    let registerType<'a> (reader: byte[] -> obj) (writer: obj -> byte[]) =
        if transformers.ContainsKey(typeof<'a>) then
            invalidOp (sprintf "Type %s has already been registered." typeof<'a>.Name)
        transformers.Add(typeof<'a>, (reader, writer))
        
            
    [<EditorBrowsableAttribute(EditorBrowsableState.Never)>]
    [<CompilerMessageAttribute("This method is intended for use in generated code only.", 10001, IsHidden=true, IsError=false)>]
    let internal readLeaf<'a> path indices obj =
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
    let internal writeLeaf<'a> path indices (attrVal: 'a option) obj =
        match transformers.TryGetValue(typeof<'a>) with
        | (true, (_, writer)) ->
            attrVal
            |> Option.map (box >> writer >> box)
            |> writePath path indices obj
        | _ ->
            invalidArg "'a" (sprintf "Type %s has not been registered. Please, use YANGProviderRuntime.registerType to register a new type." typeof<'a>.Name)


    [<EditorBrowsableAttribute(EditorBrowsableState.Never)>]
    [<CompilerMessageAttribute("This method is intended for use in generated code only.", 10001, IsHidden=true, IsError=false)>]
    let internal readLeafList<'a> path indices obj =
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
    let internal writeLeafList<'a> path indices (attrVal: 'a list option) obj =
        match transformers.TryGetValue(typeof<'a>) with
        | (true, (_, writer)) ->
            attrVal
            |> Option.map ((List.map (box >> writer)) >> box)
            |> writePath path indices obj
        | _ ->
            invalidArg "'a" (sprintf "Type %s has not been registered. Please, use YANGProviderRuntime.registerType to register a new type." typeof<'a>.Name)


    [<EditorBrowsableAttribute(EditorBrowsableState.Never)>]
    [<CompilerMessageAttribute("This method is intended for use in generated code only.", 10001, IsHidden=true, IsError=false)>]
    let internal getListLength path indices obj =
        match readPath path indices obj with
        | Some (List (_, lst)) -> List.length lst
        | Some _ -> invalidArg "path" "The given path does not point to a list."
        | None -> 0


    [<EditorBrowsableAttribute(EditorBrowsableState.Never)>]
    [<CompilerMessageAttribute("This type is intended for use in generated code only.", 10001, IsHidden=true, IsError=false)>]
    type PathBuilder =
        private {
            Object: CPSObject;
            Segments: AttributePathSegment list;
            Indices: int list
        }
        with

            static member internal Invalid = { Object = Unchecked.defaultof<_>; Segments = []; Indices = [] }

            static member FromObject obj =
                { Object = obj; Segments = []; Indices = [] }

            member this.CPSObject
                with get() = this.Object

            member this.Access path =
                { this with Segments = Access (CPSPath(path).AttributeID.Value) :: this.Segments }

            member this.Indexer i =
                { this with Segments = Indexer :: this.Segments;
                            Indices = i :: this.Indices }

            member this.ReadLeaf<'T> () =
                readLeaf<'T> (List.rev this.Segments) (List.rev this.Indices) this.Object

            member this.WriteLeaf<'T> v =
                writeLeaf<'T> (List.rev this.Segments) (List.rev this.Indices) v this.Object

            member this.ReadLeafList<'T> () =
                readLeafList<'T> (List.rev this.Segments) (List.rev this.Indices) this.Object

            member this.WriteLeafList<'T> v =
                writeLeafList<'T> (List.rev this.Segments) (List.rev this.Indices) v this.Object

            member this.GetListLength () =
                getListLength (List.rev this.Segments) (List.rev this.Indices) this.Object


    // ----------------------------------------------------------------------------------------
    // ----------------------------------------------------------------------------------------


    [<EditorBrowsableAttribute(EditorBrowsableState.Never)>]
    [<CompilerMessageAttribute("This type is intended for use in generated code only.", 10001, IsHidden=true, IsError=false)>]
    let validateObject (obj: CPSObject) (path: string) =
        if obj.Key.Key = CPSKey(CPSQualifier.Target, CPSPath path).Key then
            Ok (PathBuilder.FromObject obj)
        else
            Error MismatchingKey
