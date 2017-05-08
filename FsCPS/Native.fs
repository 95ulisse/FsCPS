namespace FsCPS

open System

type NativeCPSException(msg) =
    inherit Exception(msg)


type CPSQualifier =
    | Target = 1
    | Observed = 2
    | Proposed = 3
    | RealTime = 4


type CPSAttributeType =
    | UInt16 = 0
    | UInt32 = 1
    | UInt64 = 2
    | Binary = 3


type CPSOperationType =
    | Delete = 1
    | Create = 2
    | Set = 3
    | Action = 4



// --------------------------------------------------------------------------------
// --------------------------------------------------------------------------------
// --------------------------------------------------------------------------------



namespace FsCPS.Native

#nowarn "9" // Unverifiable IL

open System
open System.Net
open System.Runtime.InteropServices
open System.Text
open FsCPS


module internal Constants =
    [<Literal>]
    let KeyLength = 256
    [<Literal>]
    let ObjEventLength = 50000


[<AbstractClass>]
type internal SafeHandleZeroIsInvalid(handle: IntPtr, owns: bool) =
    inherit SafeHandle(handle, owns)

    override this.IsInvalid
        with get() = this.handle = IntPtr.Zero


// Definition of native types.
// Note that complex structures are modeled as classes, not structs:
// the native APIs always expect a pointer, so using a reference type helps
// avoiding manual pinning and continuous copies when these types are passed
// in the managed code.


type internal NativeAttr = nativeint

type internal NativeServerCallback<'TParam when 'TParam : not struct> =
    delegate of nativeint * 'TParam * unativeint -> int


[<Class>]
[<StructLayout(LayoutKind.Sequential)>]
type internal NativeObjectIterator =
    val mutable len: unativeint
    val mutable attr: NativeAttr

    new () =
        { len = 0un; attr = 0n }


[<Class>]
[<StructLayout(LayoutKind.Sequential)>]
type internal NativeGetParams =
    val mutable keys: nativeint // Pointer to array of Key
    val mutable key_count: unativeint
    val mutable list: nativeint
    val mutable filters: nativeint
    val mutable timeout: unativeint

    new () =
        { keys = 0n; key_count = 0un; list = 0n; filters = 0n; timeout = 0un }


[<Class>]
[<StructLayout(LayoutKind.Sequential)>]
type internal NativeTransactionParams =

    val mutable change_list: nativeint
    val mutable prev: nativeint
    val mutable timeout: unativeint

    new () =
        { change_list = 0n; prev = 0n; timeout = 0un }

[<Class>]
[<StructLayout(LayoutKind.Sequential)>]
type internal NativeEventRegistration =

    val mutable priority: int
    val mutable objects: nativeint
    val mutable objects_size: unativeint

    new () =
        { priority = 0; objects = 0n; objects_size = 0un }


[<Class>]
[<StructLayout(LayoutKind.Sequential)>]
type internal NativeServerRegistrationRequest =

    val mutable handle: nativeint
    val mutable context: nativeint
    [<MarshalAs(UnmanagedType.ByValArray, SizeConst = Constants.KeyLength)>]
    val mutable key: byte[]
    [<MarshalAs(UnmanagedType.FunctionPtr)>]
    val mutable read_function: NativeServerCallback<NativeGetParams>
    [<MarshalAs(UnmanagedType.FunctionPtr)>]
    val mutable write_function: NativeServerCallback<NativeTransactionParams>
    [<MarshalAs(UnmanagedType.FunctionPtr)>]
    val mutable rollback_function: NativeServerCallback<NativeTransactionParams>

    new () =
        { handle = 0n; context = 0n; key = null; read_function = null; write_function = null; rollback_function = null }


// Definition of native handles.
// We could have used `IntPtr`s, but a `SafeHandle` guarantees stronger release semantics,
// and implements `IDisposable`.


type internal NativeKey(handle, owns) =
    inherit SafeHandleZeroIsInvalid(handle, owns)

    new () =
        new NativeKey(IntPtr.Zero, true)

    override this.ReleaseHandle() =
        Marshal.FreeHGlobal this.handle
        true

    static member AllocateNew() =
        new NativeKey(Marshal.AllocHGlobal(Constants.KeyLength), true)


and internal NativeObject(handle, owns) =
    inherit SafeHandleZeroIsInvalid(handle, owns)

    new () =
        new NativeObject(IntPtr.Zero, true)

    override this.ReleaseHandle() =
        NativeMethods.DestroyObject this.handle
        true


and internal NativeObjectList(handle, owns) =
    inherit SafeHandleZeroIsInvalid(handle, owns)

    new () =
        new NativeObjectList(IntPtr.Zero, true)

    override this.ReleaseHandle() =
        NativeMethods.DestroyObjectList this.handle true
        true


and internal NativeEventHandle(handle) =
    inherit SafeHandleZeroIsInvalid(handle, true)

    new () =
        new NativeEventHandle(IntPtr.Zero)

    override this.ReleaseHandle() =
        NativeMethods.DestroyEventHandle this.handle
        true


/// Definitions of all the native methods needed by FsCPS.
and [<AbstractClass; Sealed>] internal NativeMethods private () =

    [<Literal>]
    static let cpsLibrary = "libcps-api-common.so"

    // Library initialization

    [<DllImport(cpsLibrary)>]
    static extern void cps_api_class_map_init()

    // Objects

    [<DllImport(cpsLibrary, CharSet = CharSet.Ansi)>]
    static extern NativeObject cps_api_object_create_int(string desc, uint32 line, string name)

    [<DllImport(cpsLibrary)>]
    static extern void cps_api_object_delete(nativeint obj)

    [<DllImport(cpsLibrary)>]
    static extern bool cps_api_object_clone(NativeObject dest, NativeObject src)

    [<DllImport(cpsLibrary)>]
    static extern nativeint cps_api_object_key(NativeObject obj)

    [<DllImport(cpsLibrary)>]
    static extern void cps_api_object_set_key(NativeObject obj, NativeKey key)

    [<DllImport(cpsLibrary)>]
    static extern bool cps_api_object_reserve(NativeObject obj, unativeint size)

    // Object attributes

    [<DllImport(cpsLibrary)>]
    static extern bool cps_api_object_e_add(
        NativeObject obj,
        [<MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2s)>] uint64[] aid,
        unativeint id_size,
        CPSAttributeType atype,
        [<MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 5s)>] byte[] data,
        unativeint len
    )

    [<DllImport(cpsLibrary)>]
    static extern void cps_api_object_attr_delete(NativeObject obj, uint64 aid)

    [<DllImport(cpsLibrary)>]
    static extern uint64 cps_api_object_attr_id(NativeAttr attr)

    [<DllImport(cpsLibrary)>]
    static extern nativeint cps_api_object_attr_data_bin(NativeAttr attr)

    [<DllImport(cpsLibrary)>]
    static extern unativeint cps_api_object_attr_len(NativeAttr attr)
        
    [<DllImport(cpsLibrary)>]
    static extern NativeAttr cps_api_object_attr_get(NativeObject obj, uint64 aid)

    [<DllImport(cpsLibrary)>]
    static extern void cps_api_object_it_begin(NativeObject obj, [<In; Out>] NativeObjectIterator it)

    // Keys

    [<DllImport(cpsLibrary)>]
    static extern bool cps_api_key_from_attr_with_qual(NativeKey key, uint64 aid, CPSQualifier qual)

    [<DllImport(cpsLibrary, CharSet = CharSet.Ansi)>]
    static extern bool cps_api_key_from_string(NativeKey key, [<MarshalAs(UnmanagedType.LPStr)>] string str)

    [<DllImport(cpsLibrary, CharSet = CharSet.Ansi)>]
    static extern nativeint cps_api_key_print(NativeKey key, [<MarshalAs(UnmanagedType.LPStr)>] StringBuilder buffer, unativeint len)

    // Attribute ID to name mapping

    [<DllImport(cpsLibrary, EntryPoint = "_Z21cps_dict_find_by_namePKc", CharSet = CharSet.Ansi)>]
    static extern nativeint cps_dict_find_by_name(string path)

    [<DllImport(cpsLibrary)>]
    static extern nativeint cps_attr_id_to_name(uint64 id)

    // Object lists

    [<DllImport(cpsLibrary)>]
    static extern NativeObjectList cps_api_object_list_create()

    [<DllImport(cpsLibrary)>]
    static extern void cps_api_object_list_destroy(nativeint list, bool delete_objects)

    [<DllImport(cpsLibrary)>]
    static extern nativeint cps_api_object_list_get(NativeObjectList list, unativeint i)

    [<DllImport(cpsLibrary)>]
    static extern bool cps_api_object_list_append(NativeObjectList list, NativeObject obj)

    [<DllImport(cpsLibrary)>]
    static extern unativeint cps_api_object_list_size(NativeObjectList list)

    // Transactions

    [<DllImport(cpsLibrary)>]
    static extern int cps_api_transaction_init([<In; Out>] NativeTransactionParams trans)

    [<DllImport(cpsLibrary)>]
    static extern int cps_api_transaction_close([<In; Out>] NativeTransactionParams trans)

    [<DllImport(cpsLibrary)>]
    static extern int cps_api_get([<In; Out>] NativeGetParams req)

    [<DllImport(cpsLibrary)>]
    static extern int cps_api_create([<In; Out>] NativeTransactionParams trans, NativeObject obj)

    [<DllImport(cpsLibrary)>]
    static extern int cps_api_set([<In; Out>] NativeTransactionParams trans, NativeObject obj)

    [<DllImport(cpsLibrary)>]
    static extern int cps_api_delete([<In; Out>] NativeTransactionParams trans, NativeObject obj)

    [<DllImport(cpsLibrary)>]
    static extern int cps_api_get_request_init([<In; Out>] NativeGetParams req)

    [<DllImport(cpsLibrary)>]
    static extern int cps_api_get_request_close([<In; Out>] NativeGetParams req)

    [<DllImport(cpsLibrary)>]
    static extern int cps_api_commit([<In; Out>] NativeTransactionParams trans)

    // API for servers

    [<DllImport(cpsLibrary)>]
    static extern int cps_api_operation_subsystem_init(nativeint pointer_to_handle, int threads)

    [<DllImport(cpsLibrary)>]
    static extern int cps_api_register([<In>] NativeServerRegistrationRequest req)

    [<DllImport(cpsLibrary)>]
    static extern int cps_api_object_type_operation(NativeKey key)

    // Events

    [<DllImport(cpsLibrary)>]
    static extern int cps_api_event_service_init()

    [<DllImport(cpsLibrary)>]
    static extern int cps_api_event_client_connect(NativeEventHandle& handle)

    [<DllImport(cpsLibrary)>]
    static extern int cps_api_event_client_disconnect(nativeint handle)

    [<DllImport(cpsLibrary)>]
    static extern int cps_api_event_client_register(NativeEventHandle handle, [<In>] NativeEventRegistration reg)

    [<DllImport(cpsLibrary)>]
    static extern int cps_api_wait_for_event(NativeEventHandle handle, NativeObject obj)

    [<DllImport(cpsLibrary)>]
    static extern int cps_api_event_publish(NativeEventHandle handle, NativeObject obj)


    // Initializes the native library as soon as the module is loaded.
    do
        cps_api_class_map_init()

    /// Returns a string representation of a CPS error code.
    static member private ReturnValueToString ret =
        match ret with
        | 0 -> "OK"
        | 1 -> "GENERIC_ERROR"
        | 2 -> "NO_SERVICE"
        | 3 -> "SERVICE_CONNECT_FAIL"
        | 4 -> "INTERNAL_FAILURE"
        | 5 -> "TIMEOUT"
        | _ -> "UNKNOWN_ERROR"

    /// Returns the string representation of the given string.
    static member PrintKey key =
        let sb = StringBuilder(1024)
        cps_api_key_print(key, sb, 1024un) |> ignore
        sb.ToString()

    /// Parses the given string representation of a key into an actual key.
    static member ParseKey str =
        let k = NativeKey.AllocateNew()
        if cps_api_key_from_string(k, str) then
            Ok k
        else
            k.Dispose()
            Error "Could not set parse key."

    /// Converts a human-readable string path to an Attribute ID.
    static member AttrIdFromPath path =
        let ptr = cps_dict_find_by_name(path)
        if ptr = IntPtr.Zero then
            Error ("Cannot find path " + path)
        else
            // The offset 40 was found by trial and error.
            // The pointer returned by `cps_dict_find_by_name` points to an internal C++ struct
            // which we don't know. This is now 40, but can change without notice.
            // This is horrible. There must be a better way. There must be.
            let id = uint64 (Marshal.ReadInt64(ptr, 40))
            Ok id

    /// Converts an attribute ID to a path name.
    static member AttrIdToPath id =
        let ptr = cps_attr_id_to_name(id)
        if ptr = IntPtr.Zero then
            Error (sprintf "Cannot find attribute with id %d" id)
        else
            Ok (Marshal.PtrToStringAnsi(ptr))

    /// Creates a native key out of a qualifier and a path.
    static member CreateKey qual path =
        NativeMethods.AttrIdFromPath path
        >>= (fun attrId ->
            let k = NativeKey.AllocateNew()
            if cps_api_key_from_attr_with_qual(k, attrId, qual) then
                Ok k
            else
                k.Dispose()
                Error "Could not set object's key."
        )

    /// Extracts the key of an object.
    static member GetObjectKey obj =
        let k = cps_api_object_key(obj)
        if k = IntPtr.Zero then
            Error "Could not get object's key."
        else
            Ok(new NativeKey(k, false))

    /// Sets an object's key.
    static member SetObjectKey key obj =
        NativeMethods.GetObjectKey obj
        >>= (fun k ->
            let res =
                if cps_api_key_from_string(k, key) then
                    Ok obj
                else
                    Error "Could not parse key."
            k.Dispose()
            res
        )

    /// Extracts the operation type from a key.
    static member GetOperationFromKey key =
        let ret = cps_api_object_type_operation(key)
        if ret = 0 then
            Error "Cannot extract operation type from key."
        else
            Ok (enum<CPSOperationType> ret)

    /// Creates a new CPS object.
    static member CreateObject () =
        let obj = cps_api_object_create_int("", 0u, "")
        if obj.IsInvalid then
            Error "Cannot create native CPS object"
        else
            Ok obj

    /// Destroys a native CPS object.
    static member DestroyObject obj =
        cps_api_object_delete(obj)

    /// Clones an object into another.
    static member CloneObject dest src =
        if cps_api_object_clone(dest, src) then
            Ok ()
        else
            Error "Could not clone object."
       
    /// Reserves the given amount of space in an object.
    static member ReserveSpaceInObject size obj =
        if cps_api_object_reserve(obj, size) then
            Ok ()
        else
            Error "Could not reseve space in object."

    /// Checks if an iterator is valid or not.
    static member private IteratorIsValid (it: NativeObjectIterator) =
        if it.attr = IntPtr.Zero then
            false
        else
            let len = Marshal.ReadInt64(it.attr, sizeof<uint64>)
            let totalLen = unativeint (len + 2L * (int64 sizeof<uint64>))
            it.len >= totalLen

    /// Advances the given iterator.
    static member private IteratorNext (it: NativeObjectIterator) =
        if it.attr <> IntPtr.Zero then
            let len = Marshal.ReadInt64(it.attr, sizeof<uint64>)
            let totalLen = unativeint (len + 2L * (int64 sizeof<uint64>))
            if it.len < totalLen then
                it.attr <- IntPtr.Zero
            else
                it.len <- it.len - totalLen
                it.attr <- it.attr + nativeint totalLen

    /// Returns a sequence that iterates over all the attributes in the given object.
    static member IterateAttributes obj =
        // Creates an iterator
        let it = NativeObjectIterator()
        cps_api_object_it_begin(obj, it)

        // Walks the iterator
        seq {
            while NativeMethods.IteratorIsValid it do
                yield cps_api_object_attr_id(it.attr)
                NativeMethods.IteratorNext(it)
        }

    /// Returns a sequence that iterates over all the native objects in the given list.
    static member IterateObjectList l =
        let len = cps_api_object_list_size(l)
        if len = 0un then
            Seq.empty
        else
            seq {
                for i = 0un to len - 1un do
                    yield new NativeObject(cps_api_object_list_get(l, i), false)
            }

    /// Adds a binary attribute to a native CPS object.
    /// On success, returns the handle to the native memory added to the native object.
    static member AddAttribute obj aid (value: byte[]) =
        if cps_api_object_e_add(obj, [| aid |], 1un, CPSAttributeType.Binary, value, unativeint value.Length) then
            Ok ()
        else
            Error "Cannot add attribute to native object."

    /// Returns a copy of the attribute store in the given object with the given id.
    static member GetAttribute obj aid =
        let attr = cps_api_object_attr_get(obj, aid)
        if attr = IntPtr.Zero then
            Error (sprintf "Could not find attribute with id %d." aid)
        else
            let len = int (cps_api_object_attr_len(attr))
            let arr = Array.zeroCreate<byte> len
            Marshal.Copy(cps_api_object_attr_data_bin(attr), arr, 0, len)
            Ok(arr)

    /// Removes the attribute from the given object.
    static member RemoveAttribute obj aid =
        cps_api_object_attr_delete(obj, aid)
        Ok()

    /// Creates a new object list.
    static member CreateObjectList () =
        let list = cps_api_object_list_create()
        if list.IsInvalid then
            Error "Cannot create native object list."
        else
            Ok list

    /// Destroys an object list.
    static member DestroyObjectList list destroyObjects =
        cps_api_object_list_destroy(list, destroyObjects)

    /// Appends an element to an object list.
    /// Note that this will mark the object being added as invalid,
    /// since its lifetime is now forever bound to the list.
    /// This is just a common append operation, but the name `Move` is to make
    /// this ownership passage more evident.
    static member MoveObjectIntoList list obj =
        if cps_api_object_list_append(list, obj) then
            obj.SetHandleAsInvalid()
            Ok list
        else
            Error "Cannot append native object to list."

    /// Extracts the object at a given index of a native object list.
    static member ObjectListGet list index =
        let ret = cps_api_object_list_get(list, index)
        if ret = IntPtr.Zero then
            Error (sprintf "Cannot get item %d of native object list." index)
        else
            Ok(new NativeObject(ret, false))

    /// Creates a new CPS Get request.
    static member CreateGetRequest () =
        let req = NativeGetParams()
        let ret = cps_api_get_request_init(req)
        if ret = 0 then
            Ok req
        else
            Error (sprintf "Cannot inizialize native get request (Return value: %s = %d)." (NativeMethods.ReturnValueToString ret) ret)

    /// Destroys the Get request.
    static member DestroyGetRequest req =
        let ret = cps_api_get_request_close(req)
        if ret = 0 then
            Ok()
        else
            Error (sprintf "Error freeing get request (Return value: %s = %d)." (NativeMethods.ReturnValueToString ret) ret)

    // Sends the given get request.
    static member GetRequestSend req =
        let ret = cps_api_get(req)
        if ret = 0 then
            Ok()
        else
            Error (sprintf "Error sending get request (Return value: %s = %d)." (NativeMethods.ReturnValueToString ret) ret)

    /// Creates a new CPS transaction.
    static member CreateTransaction () =
        let trans = NativeTransactionParams()
        let ret = cps_api_transaction_init(trans)
        if ret = 0 then
            Ok trans
        else
            Error (sprintf "Cannot initialize native transaction (Return value: %s = %d)." (NativeMethods.ReturnValueToString ret) ret)

    /// Releases the resources of a native CPS transaction.
    static member DestroyTransaction trans =
        let ret = cps_api_transaction_close(trans)
        if ret = 0 then
            Ok()
        else
            Error (sprintf "Error freeing transaction (Return value: %s = %d)." (NativeMethods.ReturnValueToString ret) ret)

    /// Adds a `create` operation to a transaction.
    static member TransactionAddCreate trans obj =
        let ret = cps_api_create(trans, obj)
        if ret = 0 then
            Ok()
        else
            Error (sprintf "Error adding operation to transaction (Return value: %s = %d)." (NativeMethods.ReturnValueToString ret) ret)

    /// Adds a `set` operation to a transaction.
    static member TransactionAddSet trans obj =
        let ret = cps_api_set(trans, obj)
        if ret = 0 then
            Ok()
        else
            Error (sprintf "Error adding operation to transaction (Return value: %s = %d)." (NativeMethods.ReturnValueToString ret) ret)

    /// Adds a `delete` operation to a transaction.
    static member TransactionAddDelete trans obj =
        let ret = cps_api_delete(trans, obj)
        if ret = 0 then
            Ok()
        else
            Error (sprintf "Error adding operation to transaction (Return value: %s = %d)." (NativeMethods.ReturnValueToString ret) ret)

    /// Commits a transaction.
    static member TransactionCommit trans =
        let ret = cps_api_commit(trans)
        if ret = 0 then
            Ok()
        else
            Error (sprintf "Error committing transaction (Return value: %s = %d)." (NativeMethods.ReturnValueToString ret) ret)

    /// Initializes the operation subsystem.
    static member InitializeOperationSubsystem () =
        let handlePtr = Marshal.AllocHGlobal(Marshal.SizeOf(typeof<IntPtr>))
        let ret = cps_api_operation_subsystem_init(handlePtr, 1)
        let handle = Marshal.ReadIntPtr(handlePtr)
        Marshal.FreeHGlobal(handlePtr)
        if ret = 0 then
            Ok handle
        else
            Error (sprintf "Error initializing operation subsystem (Return value: %s = %d)." (NativeMethods.ReturnValueToString ret) ret)

    /// Registers listeners for the given key.
    static member RegisterServer handle key read write rollback =
        
        // Prepares the request
        let req =
            NativeServerRegistrationRequest(
                handle = handle,
                context = 0n,
                key = Array.zeroCreate Constants.KeyLength,
                read_function = read,
                write_function = write,
                rollback_function = rollback
            )
        NativeMethods.ParseKey key
        |>> (fun k ->
            Marshal.Copy(k.DangerousGetHandle(), req.key, 0, Constants.KeyLength)
            k.Dispose()
        )

        // Registers the server
        >>= (fun () ->
            let ret = cps_api_register(req)
            if ret = 0 then
                Ok ()
            else
                Error (sprintf "Cannot register server (Return value: %s = %d)." (NativeMethods.ReturnValueToString ret) ret)
        )

    /// Initializes the event subsystem and starts the event thread.
    static member InitializeEventSubsystem () =
        let ret = cps_api_event_service_init()
        if ret = 0 then
            Ok ()
        else
            Error (sprintf "Cannot initialize event service (Return value: %s = %d)." (NativeMethods.ReturnValueToString ret) ret)

    /// Creates a new handle on which CPS events can be listened to.
    static member CreateEventHandle () =
        let mutable handle = Unchecked.defaultof<_>
        let ret = cps_api_event_client_connect(&handle)
        if ret = 0 then
            Ok handle
        else
            Error (sprintf "Cannot create event handle (Return value: %s = %d)." (NativeMethods.ReturnValueToString ret) ret)

    /// Destroys an event handle.
    static member DestroyEventHandle handle =
        cps_api_event_client_disconnect(handle) |> ignore

    /// Registers a key as a filter for an event handle.
    static member RegisterEventFilter handle (key: NativeKey) =
        if key.IsInvalid then
            Error "Invalid key."
        else
            // Registers the key on the handle
            let reg =
                NativeEventRegistration(
                    priority = 0,
                    objects = key.DangerousGetHandle(),
                    objects_size = 1un
                )
            let ret = cps_api_event_client_register(handle, reg)
            if ret = 0 then
                Ok ()
            else
                Error (sprintf "Cannot register native event (Return value: %s = %d)." (NativeMethods.ReturnValueToString ret) ret)

    /// Suspends the current thread waiting for a new event to arrive.
    /// When an event arrives, the given object is filled with details.
    /// Returns the same object (but filled) for chaining.
    static member WaitForEvent handle obj =
        let ret = cps_api_wait_for_event(handle, obj)
        if ret = 0 then
            Ok obj
        else
            Error (sprintf "Cannot receive event (Return value: %s = %d)." (NativeMethods.ReturnValueToString ret) ret)

    /// Publishes an event to the CPS system.
    static member PublishEvent handle obj =
        let ret = cps_api_event_publish(handle, obj)
        if ret = 0 then
            Ok ()
        else
            Error (sprintf "Cannot publish event (Return value: %s = %d)." (NativeMethods.ReturnValueToString ret) ret)