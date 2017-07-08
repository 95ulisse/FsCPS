namespace FsCPS

open System
open System.Collections.Generic
open System.Collections.ObjectModel
open System.Runtime.InteropServices
open System.Text
open System.Threading
open Microsoft.FSharp.Reflection
open FsCPS.Native


/// Human-readable absolute path of a CPS attribute.
type CPSPath = CPSPath of string
    with
        
        /// Appends a relative path to this one.
        member this.Append s =
            if isNull s then
                nullArg "s"

            let (CPSPath rawPath) = this
            CPSPath(rawPath + "/" + s)

        override this.ToString() =
            let (CPSPath rawPath) = this
            rawPath


/// Key for the CPS system.
/// A key can be constructed from its string form, or from a qualifier and a path.
/// Note that when constructing a key from its string representation,
/// the properties `Qualifier` and `Path` are not available.
type CPSKey private (key, qual, path) =

    /// Constructs a new key from a qualifier and a path.
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

    /// Constructs a new key from its string representation.
    /// Note that this will make the properties `Qualifier` and `Path` unavailable.
    new (key) =

        // Validates the key
        NativeMethods.ParseKey key
        |>> (fun k -> k.Dispose())
        |> Result.okOrThrow (invalidArg "key")

        CPSKey(key, None, None)

    /// String representation of the key.
    member val Key: string = key

    /// Qualifier of the key.
    /// Is `None` if the key was constructed from a string.
    member val Qualifier: CPSQualifier option = qual

    /// Path of the key.
    /// Is `None` if the key was constructed from a string.
    member val Path: CPSPath option = path

    override this.ToString() =
        key


/// Object of the CPS system.
type CPSObject(key: CPSKey) =

    let attributes = Dictionary<CPSPath, CPSAttribute>()

    /// Constructs a new object with a key with the given path.
    /// The default qualifier is `CPSQualifier.Target`.
    new (rootPath) =
        CPSObject(CPSKey(CPSQualifier.Target, rootPath))

    /// Constructs a new object with a key with the given path and qualifier.
    new (rootPath, qual) =
        CPSObject(CPSKey(qual, rootPath))

    /// Key of the object.
    member val Key = key

    /// Read-only view of the attributes of this object as a dictionary.
    /// Attributes are indexed by their path.
    member val Attributes = attributes.Values

    /// Sets the value of an attribute.
    /// Note that `name` is expected to be a path relative to the object's key.
    member this.SetAttribute(name: string, value: byte[]) =
        match this.Key.Path with
        | Some path -> this.SetAttribute(path.Append(name), value)
        | None ->
            invalidOp (
                "Cannot use a relative path to get/set attributes, " +
                "since this object has been constructed with a partial key. " +
                "Use the overload accepting an absolute CPSPath."
            )

    /// Sets the value of an attribute.
    /// Note that `name` is expected to be a path relative to the object's key.
    member this.SetAttribute(name: string, value: byte[] list) =
        match this.Key.Path with
        | Some path -> this.SetAttribute(path.Append(name), value)
        | None ->
            invalidOp (
                "Cannot use a relative path to get/set attributes, " +
                "since this object has been constructed with a partial key. " +
                "Use the overload accepting an absolute CPSPath."
            )

    /// Sets the value of an attribute.
    /// Note that `name` is expected to be a path relative to the object's key.
    member this.SetAttribute(name: string, value: CPSAttribute list) =
        match this.Key.Path with
        | Some path -> this.SetAttribute(path.Append(name), value)
        | None ->
            invalidOp (
                "Cannot use a relative path to get/set attributes, " +
                "since this object has been constructed with a partial key. " +
                "Use the overload accepting an absolute CPSPath."
            )

    /// Sets the value of an attribute.
    /// Note that `name` is expected to be a path relative to the object's key.
    member this.SetAttribute(name: string, value: CPSAttribute list list) =
        match this.Key.Path with
        | Some path -> this.SetAttribute(path.Append(name), value)
        | None ->
            invalidOp (
                "Cannot use a relative path to get/set attributes, " +
                "since this object has been constructed with a partial key. " +
                "Use the overload accepting an absolute CPSPath."
            )

    /// Sets the value of an attribute using its absolute path.
    member this.SetAttribute(path: CPSPath, value: byte[]) =
        attributes.[path] <- Leaf(path, value)

    /// Sets the value of an attribute using its absolute path.
    member this.SetAttribute(path: CPSPath, value: byte[] list) =
        attributes.[path] <- LeafList(path, value)

    /// Sets the value of an attribute using its absolute path.
    member this.SetAttribute(path: CPSPath, value: CPSAttribute list) =
        attributes.[path] <- Container(path, value)

    /// Sets the value of an attribute using its absolute path.
    member this.SetAttribute(path: CPSPath, value: CPSAttribute list list) =
        attributes.[path] <- List(path, value)

    /// Sets the value of an attribute using its absolute path.
    member internal this.SetAttribute(path: CPSPath, value: CPSAttribute) =
        attributes.[path] <- value

    /// Extracts an attribute from this object.
    /// Note that `name` is expected to be a path relative to the object's key.
    member this.GetAttribute(name: string) =
        match this.Key.Path with
        | Some path -> this.GetAttribute(path.Append(name))
        | None ->
            invalidOp (
                "Cannot use a relative path to get/set attributes, " +
                "since this object has been constructed with a partial key. " +
                "Use the overload accepting an absolute CPSPath."
            )

    /// Extracts an attribute using its absolute path.
    /// This method is just a convenience method to read leaf values.
    member this.GetAttribute(path: CPSPath) =
        this.GetAttribute(path, CPSAttributeClass.Leaf)
        |> Option.map (
            function
            | Leaf (_, v) -> v
            | _ -> invalidOp "Unreachable"
        )

    /// Extracts an attribute using its absolute path.
    member this.GetAttribute(path: CPSPath, c: CPSAttributeClass) =
        match attributes.TryGetValue(path) with
        | (true, a) -> Some (a.CheckClass(c) |> Result.okOrThrow invalidOp)
        | _ -> None

    /// Removes an attribute from this object.
    /// Note that `name` is expected to be a path relative to the object's key.
    member this.RemoveAttribute(name: string) =
        match this.Key.Path with
        | Some path -> this.RemoveAttribute(path.Append(name))
        | None ->
            invalidOp (
                "Cannot use a relative path to get/set attributes, " +
                "since this object has been constructed with a partial key. " +
                "Use the overload accepting an absolute CPSPath."
            )

    /// Removes an attribute using its absolute path.
    member this.RemoveAttribute(path: CPSPath) =
        attributes.Remove(path)
        
    /// Returns a string representation of this object,
    /// complete of key and attributes.
    member this.ToString(truncateArray: bool) =
        let sb = StringBuilder()
        sb.Append("Key: ").Append(this.Key.Key).Append('\n')
          .Append("Path alias: ").Append(this.Key.Path).Append('\n') |> ignore
        attributes.Values |> Seq.iteri (fun i attr ->
            sb.Append(if i > 0 then "\n" else "")
              .Append(attr.ToString(truncateArray, 0) : string) |> ignore
        )
        sb.ToString()

    /// Converts this object to its native representation.
    /// This method will pin all the attributes and return
    /// a list of all the allocated handles.
    member internal this.ToNativeObject() =
        
        // Creates a new object
        NativeMethods.CreateObject()

        // Adds all the attributes
        >>= (fun nativeObject ->
            attributes.Values
            |> foldResult (fun _ attr ->
                attr.AddToNativeObject(nativeObject, Stack<_>())
            ) ()
            |>> (fun _ -> nativeObject)
        )

        // Sets the object's key
        >>= NativeMethods.SetObjectKey this.Key.Key

    /// Constructs a new `CPSObject` from then given native object.
    /// Attributes are copied from the native object to new managed arrays.
    static member internal FromNativeObject(nativeObject: NativeObject) =
        
        // Extracts the key from the native object and construct a managed one
        NativeMethods.GetObjectKey(nativeObject)
        |>> NativeMethods.PrintKey
        |>> CPSKey
        |>> CPSObject

        // Iterate the attributes and copy them into the object
        |> Result.tee (fun o ->
            NativeMethods.BeginAttributeIterator(nativeObject).Iterate()
            |> foldResult (fun _ it ->
                CPSAttribute.FromIterator it
                |>> (fun (attr: CPSAttribute) -> o.SetAttribute(attr.Path, attr))
            ) ()
        )


/// Attribute of a CPS object.
and CPSAttribute =
    | Leaf of CPSPath * byte[]
    | LeafList of CPSPath * byte[] list
    | Container of CPSPath * CPSAttribute list
    | List of CPSPath * CPSAttribute list list
    
    with

    member this.Path =
        match this with
        | Leaf(p, _) | LeafList(p, _) | Container (p, _) | List(p, _) -> p

    member this.ToString(truncateArray: bool, indent: int) =
        let ind = String.replicate indent "    "
        let sb = StringBuilder().Append(ind).Append(this.Path.ToString()).Append(": ")
        match this with
    
        // Leaf
        | Leaf (path, arr) ->
            sb.Append("[ ") |> ignore
            for i in 0 .. (min (arr.Length - 1) (if truncateArray then 9 else Int32.MaxValue)) do
                sb.Append(if i > 0 then ", " else "").Append(arr.[i]) |> ignore
            if truncateArray && arr.Length > 10 then
                sb.Append(", ...") |> ignore
            sb.Append(" ]") |> ignore

        // LeafList
        | LeafList (path, lst) ->
            sb.Append("LeafList [") |> ignore
            lst |> List.iteri (fun i arr ->
                sb.Append(if i = 0 then "\n" else "")
                  .Append(ind + "    ")
                  .Append("[ ") |> ignore
                for j in 0 .. (min (arr.Length - 1) (if truncateArray then 9 else Int32.MaxValue)) do
                    sb.Append(if j > 0 then ", " else "").Append(arr.[j]) |> ignore
                if truncateArray && arr.Length > 10 then
                    sb.Append(", ...") |> ignore
                sb.Append(" ]\n") |> ignore
            )
            sb.Append(ind).Append("]") |> ignore

        // Container
        | Container (_, lst) ->
            sb.Append("Container {") |> ignore
            lst |> List.iteri (fun i x ->
                sb.Append(if i = 0 then "\n" else "")
                    .Append(x.ToString(truncateArray, indent + 1) : string)
                    .Append("\n")|> ignore
            )
            sb.Append(ind).Append("}") |> ignore

        // List
        | List (_, lst) ->
            sb.Append("List [") |> ignore
            lst |> List.iteri (fun i el ->
                sb.Append(if i = 0 then "\n" else "")
                  .Append(ind + "    ")
                  .Append("{") |> ignore
                el |> List.iteri (fun j x ->
                    sb.Append(if j = 0 then "\n" else "")
                      .Append(x.ToString(truncateArray, indent + 2) : string)
                      .Append("\n")|> ignore
                )
                sb.Append(ind + "    ").Append("}\n") |> ignore
            )
            sb.Append(ind).Append("]") |> ignore

        | _ -> raise (NotImplementedException("Missing stringification of attribute"))

        sb.ToString()

    member internal this.CheckClass(c: CPSAttributeClass) =
        match this, c with
        | Leaf _, CPSAttributeClass.Leaf -> Ok this
        | LeafList _, CPSAttributeClass.LeafList -> Ok this
        | Container _, CPSAttributeClass.Container -> Ok this
        | List _, CPSAttributeClass.List -> Ok this
        | _ ->
            Error (
                sprintf "Attribute with path %s is a %s, but given a %s."
                        (this.Path.ToString())
                        (Enum.GetName(typeof<CPSAttributeClass>, c))
                        (match FSharpValue.GetUnionFields(this, typeof<CPSAttribute>) with (info, _) -> info.Name)
            )

    member internal this.AddToNativeObject(obj: NativeObject, ids: Stack<NativeAttrID>) =

        // Extracts attribute ID
        NativeMethods.AttrIdFromPath(this.Path.ToString())

        // Checks that the class of the attribute matches the current case of the union
        |> Result.tee (fun attrId ->
            NativeMethods.AttrClassFromAttrId(attrId)
            >>= this.CheckClass
        )

        >>= fun attrId ->
            match this with
            | Leaf (_, arr) ->
                
                // For the leafs, just add the value to the object
                ids.Push(attrId)
                let aid = ids.ToArray()
                ids.Pop() |> ignore
                NativeMethods.AddNestedAttribute obj aid arr

            | LeafList (_, lst) ->

                ids.Push(attrId)
                let aid = ids.ToArray()
                ids.Pop() |> ignore

                // Be sure that the list gets added to the object, even though it's empty
                match lst with
                | [] -> NativeMethods.AddNestedAttribute obj aid Array.empty<_>
                | _ -> lst |> foldResult (fun () arr -> NativeMethods.AddNestedAttribute obj aid arr) ()

            | Container (_, lst) ->

                // Add all the attributes of the container under this one
                ids.Push(attrId)
                lst
                |> foldResult (fun () attr -> attr.AddToNativeObject(obj, ids)) ()
                |>> (fun _ -> ids.Pop() |> ignore)

            | List (_, lst) ->

                // Iterate the list and add each single attribute
                ids.Push(attrId)
                lst |> foldResult (fun i item ->
                    ids.Push(i)
                    item
                    |> foldResult (fun () attr -> attr.AddToNativeObject(obj, ids)) ()
                    |>> (fun _ ->
                        ids.Pop() |> ignore
                        i + 1UL
                    )
                ) 0UL
                |>> (fun _ -> ids.Pop() |> ignore)

    static member internal FromIterator (it: NativeObjectIterator) =
        let attrId = NativeMethods.AttrIdFromAttr it.attr
        
        NativeMethods.AttrClassFromAttrId attrId
        |> Result.pipe (NativeMethods.AttrIdToPath attrId |>> CPSPath)
        >>= fun (c, path) ->
            match c with
            | CPSAttributeClass.Leaf ->
                
                // Read the current value
                Ok (Leaf (path, it.Value()))

            | CPSAttributeClass.LeafList ->
                
                // Leaf lists are represented as consecutive values.
                // Consume as much values as possible at once.
                // Clone the iterator to make sure to not advance the original too far.

                let itCopy = NativeObjectIterator(attr = it.attr, len = it.len)
                let mutable listLen = 0
                let lst =
                    itCopy.Iterate()
                    |> Seq.takeWhile (fun it -> (NativeMethods.AttrIdFromAttr it.attr) = attrId)
                    |> Seq.fold (fun lst it ->
                        listLen <- listLen + 1
                        it.Value() :: lst
                    ) []
                    |> List.rev
                
                // Advances the original iterator till the end of the leaf list
                for i in 0 .. (listLen - 1) do
                    it.Next()

                Ok (LeafList (path, lst))

            | CPSAttributeClass.Container ->
                
                // Containers are just a list of attributes
                it.Inside().Iterate()
                |> foldResult (fun lst innerIt ->
                    CPSAttribute.FromIterator(innerIt)
                    |>> (fun attr -> attr :: lst)
                ) []
                |>> List.rev
                |>> (fun lst -> Container (path, lst))

            | CPSAttributeClass.List ->

                // Lists are implemented in binary form as a list of lists.
                it.Inside().Iterate()
                |> foldResult (fun outerList outerIt ->
                    outerIt.Inside().Iterate()
                    |> foldResult (fun innerList innerIt ->
                        CPSAttribute.FromIterator(innerIt)
                        |>> (fun innerAttr -> innerAttr :: innerList)
                    ) []
                    |>> List.rev
                    |>> (fun innerList -> innerList :: outerList)
                ) []
                |>> List.rev
                |>> (fun lst -> List (path, lst))

            | _ ->
                invalidOp (sprintf "Unexpected enum value %d" (int c))


/// Container of a transaction for CPS objects.
/// The operations are accumulated and delayed till the actual commit.
type CPSTransaction() =

    let createObjects = ResizeArray<CPSObject>()
    let setObjects = ResizeArray<CPSObject>()
    let deleteObjects = ResizeArray<CPSObject>()

    static let addObjects (objs: seq<CPSObject>) cb context =

        // Apply the operator to all the objects in the sequence
        objs
        |> foldResult (fun _ o ->
            o.ToNativeObject()
            >>= cb context
        ) ()

        // Return the context for chaining
        |>> (fun () -> context)

    /// Adds a request for the creation of an object to this transaction.
    /// Note that the transaction is not executed yet. To commit the transaction,
    /// call the `Commit` method.
    member this.Create(o: CPSObject) =
        createObjects.Add(o)
    
    /// Adds a request for the update of an object to this transaction.
    /// Note that the transaction is not executed yet. To commit the transaction,
    /// call the `Commit` method.
    member this.Set(o: CPSObject) =
        setObjects.Add(o)

    /// Adds a request for the deletion of an object to this transaction.
    /// Note that the transaction is not executed yet. To commit the transaction,
    /// call the `Commit` method.
    member this.Delete(o: CPSObject) =
        deleteObjects.Add(o)

    /// Commits all the operations queued in this transaction.
    /// Blocks until the transaction is completed.
    member this.Commit() =

        // These references are used for cleanup
        let mutable transaction = None

        let mutable result =

            // First of all, we create a new transaction and store it
            NativeMethods.CreateTransaction()
            >>= (fun t -> transaction <- Some t; Ok(t))

            // Then we add the objects to it
            >>= addObjects createObjects NativeMethods.TransactionAddCreate
            >>= addObjects setObjects NativeMethods.TransactionAddSet
            >>= addObjects deleteObjects NativeMethods.TransactionAddDelete

            // And commit!
            >>= NativeMethods.TransactionCommit

        // Destroy the transaction.
        // This will also free all the native objects and the attributes we created earlier.
        // Note that we destroy the transaction in every case, both error and success to
        // release native memory.
        if transaction.IsSome then
            result <- NativeMethods.DestroyTransaction(transaction.Value)

        result

    /// Executes immediately a get request with the given filters.
    static member Get(filters: seq<CPSObject>) =
        
        // These references are used for cleanup
        let mutable req = None

        let readResponse (req: NativeGetParams) () =
            NativeMethods.IterateObjectList(req.list)
            |> foldResult (fun objectList nativeObject ->

                // Read the object and append the object to the list
                CPSObject.FromNativeObject nativeObject
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

            // And sends the request
            >>= NativeMethods.GetRequestSend

            // Now extract the objects from the response
            >>= readResponse req.Value

        // Destroy the transaction.
        // This will also free all the native objects and the attributes we created earlier.
        // Note that we destroy the transaction in every case, both error and success to
        // release native memory.
        if req.IsSome then
            match NativeMethods.DestroyGetRequest(req.Value), result with
            | Error e, Ok _ -> result <- Error e
            | _ -> ()

        result


/// Interface representing a CPS server able to serve requests sent from a client.
/// A server listens for requests on a certain key and its methods get called when
/// a new requests is incoming. To register a server, use `CPSServer.Register`.
type ICPSServer =

    /// Responds to get requests.
    /// The argument is an object representing the filter.
    abstract member Get: CPSObject -> Result<seq<CPSObject>, string>

    /// Responds to set requests.
    /// The arguments are an object representing the filter and the type of operation requested.
    /// This function is expected to return an object that will contain data that can be used
    /// to rollback the operation if requested.
    abstract member Set: CPSObject * CPSOperationType -> Result<CPSObject, string>

    /// Rolls back a failed transaction.
    abstract member Rollback: CPSObject -> Result<unit, string>


/// Handle for a registration of a server in the CPS system.
/// To register a CPS server use the method `CPSServer.Register`.
type CPSServerHandle internal (key: CPSKey, server: ICPSServer) =
    
    let mutable _isValid = true

    let protect f arg =
        try
            f arg
        with
        | e -> Error e.Message

    let _nativeGetCallback (_: nativeint) (req: NativeGetParams) (index: unativeint) =

        // Extracts the object from the list and converts it into a managed object
        NativeMethods.ObjectListGet req.filters index
        >>= CPSObject.FromNativeObject

        // Invokes the managed server
        >>= protect server.Get

        // Converts the results to native objects and appends them to the native list
        >>= foldResult (fun _ o ->
            o.ToNativeObject()
            |>> NativeMethods.AppendObjectToList req.list
            |>> ignore
        ) ()

        // Converts the result to an integer
        |> function
           | Ok () -> 0
           | Error _ -> 1 

    let _nativeSetCallback (_: nativeint) (req: NativeTransactionParams) (index: unativeint) =

        // Extracts the object from the list and the operation requested from its key
        NativeMethods.ObjectListGet req.change_list index
        >>= (fun o ->
            NativeMethods.GetObjectKey o
            >>= NativeMethods.GetOperationFromKey
            |>> (fun op -> (o, op))
        )

        // Convert the native object to a managed one
        >>= (fun (o, op) ->
            CPSObject.FromNativeObject(o)
            |>> (fun o -> (o, op))
        )

        // Invokes the managed server and transforms the rollback object to a native one
        >>= protect server.Set
        >>= (fun o -> o.ToNativeObject())

        // Store the rollback object in the list
        >>= (fun nativeRollbackObject ->
            
            // If there was an object already allocated, copy the returned object, otherwise
            // append directly it to the list
            match NativeMethods.ObjectListGet req.prev index with
            | Ok dest ->
                NativeMethods.CloneObject dest nativeRollbackObject
                |>> (fun _ -> NativeMethods.DestroyObject nativeRollbackObject)
            | Error _ ->
                NativeMethods.AppendObjectToList req.prev nativeRollbackObject
                |>> ignore

        )

        // Converts the result to an integer
        |> function
           | Ok () -> 0
           | Error _ -> 1
    
    let _nativeRollbackCallback (_: nativeint) (req: NativeTransactionParams) (index: unativeint) =

        // Extracts the object from the list and converts it to a managed one
        NativeMethods.ObjectListGet req.prev index
        >>= CPSObject.FromNativeObject

        // Call the server
        >>= protect server.Rollback

        // Converts the result to an integer
        |> function
           | Ok () -> 0
           | Error _ -> 1


    interface IDisposable with
        member this.Dispose() = this.Dispose()
    
    // We need to be sure that the references to the delegate themselves are kept alive,
    // so we store them in a property of this object. These delegates will be marshalled as
    // function pointers to the native code.

    member val internal NativeGetCallback: NativeServerCallback<_> = NativeServerCallback(_nativeGetCallback)
    member val internal NativeSetCallback: NativeServerCallback<_> = NativeServerCallback(_nativeSetCallback)
    member val internal NativeRollbackCallback: NativeServerCallback<_> = NativeServerCallback(_nativeRollbackCallback)

    /// The key for which the server was registered.
    member val Key = key

    /// The registered server.
    member val Server = server

    /// Returns a value indicating whether this registration is still valid.
    /// To unregister the server, use the method `Cancel`.
    member val IsValid = _isValid

    /// Unregisters the server from the CPS system and releases all the resources held.
    member this.Dispose() =
        if not _isValid then
            raise (ObjectDisposedException(typeof<CPSServerHandle>.Name))
            
        // It looks like it's not possible to unregister a server...
        _isValid <- false

        // Unregisters the native callbacks and removes the reference to this server
        //NativeMethods.UnregisterServer(...)
        //CPS.ServerHandles.Remove(this) |> ignore
        //_isValid <- false


/// A single event published by the CPS system.
and [<Struct>] CPSEvent internal (op: CPSOperationType, obj: CPSObject) =
    member this.Operation = op
    member this.Object = obj


/// Observable that fires every time a CPS event is raised.
and CPSEventObservable internal (handle: NativeEventHandle, key: CPSKey) as this =
    
    let mutable _ended = false
    let mutable _nextSubscription = 0
    let mutable _subscriptions = Map.empty<int, IObserver<_>>
    
    let _thread = Thread(fun () ->
        
        // Creates the object that will receive the events
        let obj =
            NativeMethods.CreateObject()
            |> Result.tee (NativeMethods.ReserveSpaceInObject (unativeint Constants.ObjEventLength))
            |> Result.okOrThrow invalidOp

        let mutable stop = lock this (fun () -> _ended)
        while not stop do
            let res =
                NativeMethods.WaitForEvent handle obj
                >>= (fun obj ->
                    NativeMethods.GetObjectKey obj
                    >>= NativeMethods.GetOperationFromKey
                    |> Result.pipe (CPSObject.FromNativeObject obj)
                    |>> CPSEvent
                )

            stop <- lock this (fun () -> _ended)

            // In case of a native error, raise an error and stop the observable.
            if not stop then
                match res with
                | Ok evt ->
                    this.RaiseNext evt
                | Error e ->
                    this.RaiseError (NativeCPSException e)
                    this.Dispose()
                    stop <- true

        // Cleanup
        NativeMethods.DestroyObject obj
        NativeMethods.DestroyEventHandle handle |> ignore
    )

    do
        // Starts the thread that will suspend waiting for events
        _thread.Name <- "CPS Event thread for key " + key.Key
        _thread.IsBackground <- true
        _thread.Start()

    interface IDisposable with
        member this.Dispose() = this.Dispose()

    interface IObservable<CPSEvent> with
        member this.Subscribe(observer) = this.Subscribe(observer)

    /// Key for which this observable will emit events.
    member val Key = key

    /// Returns a value indicating whether this observable is inactive.
    /// To deactivate the observable, call the `Dispose` method.
    member val IsCompleted = _ended

    member private this.RaiseNext(newValue) =
        _subscriptions
        |> Map.iter (fun _ obs ->
            try
                obs.OnNext(newValue)
            with
            | e -> this.RaiseError(e)
        )

    member private this.RaiseError(err: Exception) =
        _subscriptions
        |> Map.iter (fun _ obs ->
            obs.OnError(err)
        )

    member private this.RaiseCompleted () =
        _subscriptions
        |> Map.iter (fun _ obs ->
            try
                obs.OnCompleted()
            with
            | e -> this.RaiseError(e)
        )

    /// Notifies the provider that an observer is ready to receive notifications.
    member this.Subscribe(observer: IObserver<_>) =
        lock this (fun () ->
            if this.IsCompleted then
                raise (ObjectDisposedException(typeof<CPSEventObservable>.Name))

            let observerKey = _nextSubscription
            _nextSubscription <- _nextSubscription + 1
            _subscriptions <- _subscriptions.Add(observerKey, observer)
            {
                new IDisposable with
                    member __.Dispose() =
                        lock this (fun () ->
                            _subscriptions <- _subscriptions.Remove(observerKey)
                        )
            }
        )

    /// Releases all the native resources allocated and stops emitting events.
    /// This will emit an `OnComplete` event.
    member this.Dispose() =
        lock this (fun () ->
            if this.IsCompleted then
                raise (ObjectDisposedException(typeof<CPSEventObservable>.Name))
            
            // The native library does not provide a way to wake up a thread suspended
            // in a cps_api_wait_for_event call, so we can do nothing to stop it from the outside.
            // The thread will actually stop when the next event is generated.
            _ended <- true

        )
        this.RaiseCompleted()


/// Provides a set of methods to manage global CPS services.
and CPS private () =
    
    static let _opSubsystemHandle = lazy (
        NativeMethods.InitializeOperationSubsystem()
        |> Result.okOrThrow invalidOp
    )

    static let _eventSubsystemInitialization = lazy (
        NativeMethods.InitializeEventSubsystem()
        |> Result.okOrThrow invalidOp
    )
    
    static let _eventPublishingHandle = lazy (
        _eventSubsystemInitialization.Force()
        NativeMethods.CreateEventHandle()
        |> Result.okOrThrow invalidOp
    )

    static member val internal ServerHandles: ResizeArray<_> = ResizeArray<CPSServerHandle>()

    /// Registers a new CPS server with the given key.
    /// Returns an handle that can be used to cancel the registration
    static member RegisterServer(key: CPSKey, server: ICPSServer) =
        
        // Create a new handle and register it
        let handle = new CPSServerHandle(key, server)
        NativeMethods.RegisterServer
            _opSubsystemHandle.Value
            key.Key
            handle.NativeGetCallback
            handle.NativeSetCallback
            handle.NativeRollbackCallback

        // Keep a reference to the handle to make sure that the GC does not collect it
        // invalidating the function pointers that we passed to the native code
        |>> (fun _ ->
            CPS.ServerHandles.Add(handle)
            handle
        )

    /// Starts listening for CPS events with the given key.
    /// Returns an observable that will fire every time an event is published.
    /// Note that the subscribers of the observable will be called on another thread.
    static member ListenEvents(key: CPSKey) =
        
        // Initializes the event subsystem
        _eventSubsystemInitialization.Force() |> ignore

        // Creates a new handle for the events
        NativeMethods.CreateEventHandle()

        // Registers the key in the handle
        |> Result.pipe (NativeMethods.ParseKey key.Key)
        |> Result.tee (fun (handle, k) ->
            NativeMethods.RegisterEventFilter handle k.Address
        )

        // Disposes the native key
        |>> (fun (handle, k) ->
            k.Dispose()
            (handle, key)
        )

        // Constructs and returns the observable
        |>> CPSEventObservable


    /// Publishes an event to the CPS system.
    static member PublishEvent(obj: CPSObject) =
        obj.ToNativeObject()
        |> Result.tee (NativeMethods.PublishEvent _eventPublishingHandle.Value)
        |>> NativeMethods.DestroyObject