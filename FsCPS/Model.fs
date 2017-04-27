namespace FsCPS

open System
open System.Collections.Generic
open System.Collections.ObjectModel
open System.Runtime.InteropServices
open FsCPS.Native


/// Human-readable path of a CPS attribute.
type CPSPath = CPSPath of string
    with
        member this.Append s =
            if isNull s then
                nullArg "s"

            let (CPSPath rawPath) = this
            CPSPath(rawPath + "/" + s)

        override this.ToString() =
            let (CPSPath rawPath) = this
            rawPath


/// Key for the CPS system.
/// Contains both the qualifier and the path.
type CPSKey private (key, qual, path) =

    new (qual: CPSQualifier, path: CPSPath) =
        let key =
            NativeMethods.CreateKey qual (path.ToString())
            |>> (fun k ->
                let str = NativeMethods.PrintKey(k.Address)
                k.Dispose()
                str
            )
            |> Result.okOrThrow (invalidArg "path")
        CPSKey(key, Some qual, Some path)

    new (key) =
        CPSKey(key, None, None)

    member val Key: string = key
    member val Qualifier: CPSQualifier option = qual
    member val Path: CPSPath option = path

    override this.ToString() =
        key


/// Object of the CPS system.
type CPSObject(key: CPSKey) =

    let attributes = Dictionary<CPSPath, CPSAttribute>()

    new (rootPathStr) =
        CPSObject(CPSKey(CPSQualifier.Target, CPSPath rootPathStr))

    new (rootPath) =
        CPSObject(CPSKey(CPSQualifier.Target, rootPath))

    new (rootPath, qual) =
        CPSObject(CPSKey(qual, rootPath))

    member val Key = key
    member val Attributes = ReadOnlyDictionary(attributes)

    member this.SetAttribute(name: string, value: byte[]) =
        match this.Key.Path with
        | Some path -> this.SetAttribute(path.Append(name), value)
        | None ->
            invalidOp (
                "Cannot use a relative path to get/set attributes, " +
                "since this object has been constructed with a partial key. " +
                "Use the overload accepting an absolute CPSPath."
            )

    member this.SetAttribute(path: CPSPath, value: byte[]) =
        attributes.Add(path, CPSAttribute(this, path, value))

    member this.GetAttribute(name: string) =
        match this.Key.Path with
        | Some path -> this.GetAttribute(path.Append(name))
        | None ->
            invalidOp (
                "Cannot use a relative path to get/set attributes, " +
                "since this object has been constructed with a partial key. " +
                "Use the overload accepting an absolute CPSPath."
            )

    member this.GetAttribute(path: CPSPath) =
       match attributes.TryGetValue(path) with
       | (true, a) -> Some a
       | _ -> None

    member this.RemoveAttribute(name: string) =
        match this.Key.Path with
        | Some path -> this.RemoveAttribute(path.Append(name))
        | None ->
            invalidOp (
                "Cannot use a relative path to get/set attributes, " +
                "since this object has been constructed with a partial key. " +
                "Use the overload accepting an absolute CPSPath."
            )

    member this.RemoveAttribute(path: CPSPath) =
        attributes.Remove(path)


/// Attribute of a CPS object.
and CPSAttribute internal (obj: CPSObject, path: CPSPath, value: byte[]) =

    member val Path = path
    member val Value = value
    member val OwnerObject = obj


/// Container of a transaction for CPS objects.
/// The operations are accumulated and delayed till the actual commit.
type CPSTransaction() =

    let createObjects = ResizeArray<CPSObject>()
    let setObjects = ResizeArray<CPSObject>()
    let deleteObjects = ResizeArray<CPSObject>()

    static let addObjects objs cb storeHandle context =

        let addAttr (obj: NativeObject) _ (attr: CPSAttribute) =
            
            // Convert the path to an attribute id
            NativeMethods.AttrIdFromPath(attr.Path.ToString())

            // Add the attribute
            >>= (fun attrId ->
                NativeMethods.AddAttribute obj attrId attr.Value
            )

            // Store the GC handle in the list
            >>= (fun h -> storeHandle h; Ok())

        let objToNative (obj: CPSObject) =

            // Create a new object
            NativeMethods.CreateObject()

            //Add all the attributes
            >>= (fun nativeObject ->
                obj.Attributes.Values
                |> foldResult (addAttr nativeObject) ()
                >>= (fun () -> Ok(nativeObject))
            )

            // Sets the object's key
            >>= NativeMethods.SetObjectKey obj.Key.Key

        // Apply the operator to all the objects in the sequence
        objs
        |> foldResult (fun _ o ->
            objToNative o
            >>= cb context
        ) ()

        // Return the context for chaining
        |>> (fun () -> context)

    member this.Create(o: CPSObject) =
        createObjects.Add(o)

    member this.Set(o: CPSObject) =
        setObjects.Add(o)

    member this.Delete(o: CPSObject) =
        deleteObjects.Add(o)

    member this.Commit() =

        // These references are used for cleanup
        let mutable transaction = None
        let mutable handles = []

        let storeHandle h =
            handles <- h :: handles

        let mutable result =

            // First of all, we create a new transaction and store it
            NativeMethods.CreateTransaction()
            >>= (fun t -> transaction <- Some t; Ok(t))

            // Then we add the objects to it
            >>= addObjects createObjects NativeMethods.TransactionAddCreate storeHandle
            >>= addObjects setObjects NativeMethods.TransactionAddSet storeHandle
            >>= addObjects deleteObjects NativeMethods.TransactionAddDelete storeHandle

            // And commit!
            >>= NativeMethods.TransactionCommit

        // Destroy the transaction and release the GCHandles.
        // This will also free all the native objects and the attributes we created earlier.
        // Note that we destroy the transaction in every case, both error and success to
        // release native memory.
        if transaction.IsSome then
            result <- NativeMethods.DestroyTransaction(transaction.Value)
        handles |> List.iter (fun h -> h.Free())

        result

    static member Get(filters: seq<CPSObject>) =
        
        // These references are used for cleanup
        let mutable req = None
        let mutable handles = []
        
        let storeHandle h =
            handles <- h :: handles

        let readResponse (req: NativeGetParams) () =
            NativeMethods.IterateObjectList(req.list)
            |> foldResult (fun objectList nativeObject ->

                // Extracts the key from the native object and construct a managed one
                NativeMethods.GetObjectKey(nativeObject)
                |>> NativeMethods.PrintKey
                |>> CPSKey
                |>> CPSObject

                // Iterate the attributes and copy them into the object
                >>= (fun o ->
                    NativeMethods.IterateAttributes(nativeObject)
                    |> foldResult (fun () attrId ->
                        let path = CPSPath(NativeMethods.AttrIdToPath(attrId))
                        NativeMethods.GetAttribute nativeObject attrId
                        |>> (fun attr -> o.SetAttribute(path, attr))
                    ) ()
                    |>> (fun _ -> o)
                )

                // Append the object to the list
                |>> (fun o -> o :: objectList)

            ) []

            // Revert the list of objects to match the original order
            |>> List.rev

        let mutable result =

            // Creates a new request with the given filters
            NativeMethods.CreateGetRequest()
            >>= (fun t -> req <- Some t; Ok(t))
            >>= addObjects
                    filters
                    (fun req obj -> NativeMethods.AppendObjectToList req.filters obj |>> ignore)
                    storeHandle

            // And sends the request
            >>= NativeMethods.GetRequestSend

            // Now extract the objects from the response
            >>= readResponse req.Value

        // Destroy the transaction and release the GCHandles.
        // This will also free all the native objects and the attributes we created earlier.
        // Note that we destroy the transaction in every case, both error and success to
        // release native memory.
        if req.IsSome then
            match NativeMethods.DestroyGetRequest(req.Value), result with
            | Error e, Ok _ -> result <- Error e
            | _ -> ()
        handles |> List.iter (fun h -> h.Free())

        result