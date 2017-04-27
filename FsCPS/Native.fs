namespace FsCPS

open System


type CPSNativeException(msg: string) =
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


// Definition of native types.
// Note that complex structures are modeled as classes, not structs:
// the native APIs always expect a pointer, so using a reference type helps
// avoiding manual pinning and continuous copies when these types are passed
// in the managed code.

type internal NativeKeyStorage private (addr: nativeint) =

    interface IDisposable with
        member this.Dispose() = this.Dispose()

    new () as this =
        new NativeKeyStorage(Marshal.AllocHGlobal(256 + 8))
        then
            Marshal.WriteInt64(this.Address, 256, 42L)

    member val Address = addr

    member this.Dispose() =
        this.CheckCanary()
        Marshal.FreeHGlobal(addr)

    member this.CheckCanary() =
        let canary = Marshal.ReadInt64(addr, 256)
        if canary <> 42L then
            invalidOp (sprintf "CANARY: %d" canary)


type internal NativeObject = nativeint
type internal NativeObjectList = nativeint
type internal NativeAttr = nativeint
type internal NativeAttrID = uint64


[<Class>]
[<AllowNullLiteral>]
[<StructLayout(LayoutKind.Sequential)>]
type internal NativeObjectIterator =
    val mutable len: unativeint
    val mutable attr: NativeAttr

    new () =
        { len = 0un; attr = 0n }


[<Class>]
[<AllowNullLiteral>]
[<StructLayout(LayoutKind.Sequential)>]
type internal NativeGetParams =
    val mutable keys: nativeint // Pointer to array of Key
    val mutable key_count: unativeint
    val mutable list: NativeObjectList
    val mutable filters: NativeObjectList
    val mutable timeout: unativeint

    new () =
        { keys = 0n; key_count = 0un; list = 0n; filters = 0n; timeout = 0un }


[<Class>]
[<AllowNullLiteral>]
[<StructLayout(LayoutKind.Sequential)>]
type internal NativeTransactionParams =
    val mutable change_list: NativeObjectList
    val mutable prev: NativeObjectList
    val mutable timeout: unativeint

    new () =
        { change_list = 0n; prev = 0n; timeout = 0un }



/// Definitions of all the native methods needed by FsCPS.
module internal NativeMethods =

    // Private module to make sure that the extern functions are not directly available to the outside
    module private Extern =

        [<Literal>]
        let cpsLibrary = "libcps-api-common.so"

        // Library initialization

        [<DllImport(cpsLibrary)>]
        extern void cps_api_class_map_init()

        // Objects

        [<DllImport(cpsLibrary, CharSet = CharSet.Ansi)>]
        extern NativeObject cps_api_object_create_int(string desc, uint32 line, string name)

        [<DllImport(cpsLibrary)>]
        extern void cps_api_object_delete(NativeObject obj)

        [<DllImport(cpsLibrary)>]
        extern nativeint cps_api_object_key(NativeObject obj)

        [<DllImport(cpsLibrary)>]
        extern void cps_api_object_set_key(NativeObject obj, nativeint key)

        // Object attributes

        [<DllImport(cpsLibrary)>]
        extern bool cps_api_object_e_add(
            NativeObject obj,
            [<MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2s)>] NativeAttrID[] aid,
            unativeint id_size,
            CPSAttributeType atype,
            nativeint data,
            unativeint len
        )

        [<DllImport(cpsLibrary)>]
        extern void cps_api_object_attr_delete(NativeObject obj, NativeAttrID aid)

        [<DllImport(cpsLibrary)>]
        extern NativeAttrID cps_api_object_attr_id(NativeAttr attr)

        [<DllImport(cpsLibrary)>]
        extern nativeint cps_api_object_attr_data_bin(NativeAttr attr)

        [<DllImport(cpsLibrary)>]
        extern unativeint cps_api_object_attr_len(NativeAttr attr)
        
        [<DllImport(cpsLibrary)>]
        extern NativeAttr cps_api_object_attr_get(NativeObject obj, NativeAttrID aid)

        [<DllImport(cpsLibrary)>]
        extern void cps_api_object_it_begin(NativeObject obj, [<In; Out>] NativeObjectIterator it)

        // Keys

        [<DllImport(cpsLibrary)>]
        extern bool cps_api_key_from_attr_with_qual(nativeint key, NativeAttrID aid, CPSQualifier qual)

        [<DllImport(cpsLibrary, CharSet = CharSet.Ansi)>]
        extern bool cps_api_key_from_string(nativeint key, [<MarshalAs(UnmanagedType.LPStr)>] string str)

        [<DllImport(cpsLibrary, CharSet = CharSet.Ansi)>]
        extern nativeint cps_api_key_print(nativeint key, [<MarshalAs(UnmanagedType.LPStr)>] StringBuilder buffer, unativeint len)

        // Attribute ID to name mapping

        [<DllImport(cpsLibrary, EntryPoint = "_Z21cps_dict_find_by_namePKc", CharSet = CharSet.Ansi)>]
        extern nativeint cps_dict_find_by_name(string path)

        [<DllImport(cpsLibrary)>]
        extern string cps_attr_id_to_name(NativeAttrID id)

        // Object lists

        [<DllImport(cpsLibrary)>]
        extern NativeObjectList cps_api_object_list_create()

        [<DllImport(cpsLibrary)>]
        extern void cps_api_object_list_destroy(NativeObjectList list, bool delete_objects)

        [<DllImport(cpsLibrary)>]
        extern NativeObject cps_api_object_list_get(NativeObjectList list, unativeint i)

        [<DllImport(cpsLibrary)>]
        extern bool cps_api_object_list_append(NativeObjectList list, NativeObject obj)

        [<DllImport(cpsLibrary)>]
        extern unativeint cps_api_object_list_size(NativeObjectList list)

        // Transactions

        [<DllImport(cpsLibrary)>]
        extern int cps_api_transaction_init([<In; Out>] NativeTransactionParams trans)

        [<DllImport(cpsLibrary)>]
        extern int cps_api_transaction_close([<In; Out>] NativeTransactionParams trans)

        [<DllImport(cpsLibrary)>]
        extern int cps_api_get([<In; Out>] NativeGetParams req)

        [<DllImport(cpsLibrary)>]
        extern int cps_api_create([<In; Out>] NativeTransactionParams trans, NativeObject obj)

        [<DllImport(cpsLibrary)>]
        extern int cps_api_set([<In; Out>] NativeTransactionParams trans, NativeObject obj)

        [<DllImport(cpsLibrary)>]
        extern int cps_api_delete([<In; Out>] NativeTransactionParams trans, NativeObject obj)

        [<DllImport(cpsLibrary)>]
        extern int cps_api_get_request_init([<In; Out>] NativeGetParams req)

        [<DllImport(cpsLibrary)>]
        extern int cps_api_get_request_close([<In; Out>] NativeGetParams req)

        [<DllImport(cpsLibrary)>]
        extern int cps_api_commit([<In; Out>] NativeTransactionParams trans)


    // Initializes the native library as soon as the module is loaded.
    do
        Extern.cps_api_class_map_init()

    /// Returns a string representation of a CPS error code.
    let ReturnValueToString ret =
        match ret with
        | 0 -> "OK"
        | 1 -> "GENERIC_ERROR"
        | 2 -> "NO_SERVICE"
        | 3 -> "SERVICE_CONNECT_FAIL"
        | 4 -> "INTERNAL_FAILURE"
        | 5 -> "TIMEOUT"
        | _ -> "UNKNOWN_ERROR"

    /// Returns the string representation of the given string.
    let PrintKey key =
        let sb = StringBuilder(1024)
        Extern.cps_api_key_print(key, sb, 1024un) |> ignore
        sb.ToString()

    /// Parses the given string representation of a key into an actual key.
    let ParseKey str =
        let k = new NativeKeyStorage()
        if Extern.cps_api_key_from_string(k.Address, str) then
            k.CheckCanary()
            Ok k
        else
            Error "Could not set parse key."

    /// Converts a human-readable string path to an Attribute ID.
    let AttrIdFromPath path =
        let ptr = Extern.cps_dict_find_by_name(path)
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
    let AttrIdToPath id =
        Extern.cps_attr_id_to_name(id)

    /// Creates a native key out of a qualifier and a path.
    let CreateKey qual path =
        AttrIdFromPath path
        >>= (fun attrId ->
            let k = new NativeKeyStorage()
            if Extern.cps_api_key_from_attr_with_qual(k.Address, attrId, qual) then
                k.CheckCanary()
                Ok k
            else
                Error "Could not set object's key."
        )

    /// Extracts the key of an object.
    let GetObjectKey obj =
        let k = Extern.cps_api_object_key(obj)
        if k = IntPtr.Zero then
            Error "Could not get object's key."
        else
            Ok k

    /// Sets an object's key.
    let SetObjectKey key obj =
        GetObjectKey obj
        >>= (fun k ->
            if Extern.cps_api_key_from_string(k, key) then
                Ok obj
            else
                Error "Could not parse key."
        )

    /// Creates a new CPS object.
    let CreateObject () =
        let obj = Extern.cps_api_object_create_int("", 0u, "")
        if obj = IntPtr.Zero then
            Error "Cannot create native CPS object"
        else
            Ok obj

    /// Frees the resources of a native CPS object.
    let DestroyObject (obj: NativeObject) =
        Extern.cps_api_object_delete(obj)

    /// Checks if an iterator is valid or not.
    let IteratorIsValid (it: NativeObjectIterator) =
        if it.attr = IntPtr.Zero then
            false
        else
            let len = IPAddress.NetworkToHostOrder(Marshal.ReadInt64(it.attr, sizeof<uint64>))
            let totalLen = unativeint (len + 2L * (int64 sizeof<uint64>))
            it.len >= totalLen

    /// Advances the given iterator.
    let IteratorNext (it: NativeObjectIterator) =
        if it.attr <> IntPtr.Zero then
            let len = IPAddress.NetworkToHostOrder(Marshal.ReadInt64(it.attr, sizeof<uint64>))
            let totalLen = unativeint (len + 2L * (int64 sizeof<uint64>))
            if it.len < totalLen then
                it.attr <- IntPtr.Zero
            else
                it.len <- it.len - totalLen
                it.attr <- it.attr + nativeint totalLen

    /// Returns a sequence that iterates over all the attributes in the given object.
    let IterateAttributes (obj: NativeObject) =
        seq {
            // Creates an iterator
            let it = NativeObjectIterator()
            let handle = GCHandle.Alloc(it, GCHandleType.Pinned)
            Extern.cps_api_object_it_begin(obj, it)

            // Walks the iterator
            while IteratorIsValid it do
                yield Extern.cps_api_object_attr_id(it.attr)
                IteratorNext(it)

            handle.Free()
        }

    /// Returns a sequence that iterates over all the native objects in the given list.
    let IterateObjectList (l: NativeObjectList) =
        seq {
            let len = Extern.cps_api_object_list_size(l)
            for i = 0un to len - 1un do
                yield Extern.cps_api_object_list_get(l, i)
        }

    /// Adds a binary attribute to a native CPS object.
    /// On success, returns the handle to the native memory added to the native object.
    let AddAttribute (obj: NativeObject) (aid: NativeAttrID) (value: byte[]) =

        // Pins a copy of the array
        let handle = GCHandle.Alloc(value, GCHandleType.Pinned)

        // Add the attribute, and return the pinned handle
        if Extern.cps_api_object_e_add(obj, [| aid |], 1un, CPSAttributeType.Binary, handle.AddrOfPinnedObject(), unativeint value.Length) then
            Ok handle
        else
            Error "Cannot add attribute to native object."

    /// Returns a copy of the attribute store in the given object with the given id.
    let GetAttribute (obj: NativeObject) (aid: NativeAttrID) =
        let attr = Extern.cps_api_object_attr_get(obj, aid)
        if attr = IntPtr.Zero then
            Error (sprintf "Could not find attribute with id %d." aid)
        else
            let len = int (Extern.cps_api_object_attr_len(attr))
            let arr = Array.zeroCreate<byte> len
            Marshal.Copy(Extern.cps_api_object_attr_data_bin(attr), arr, 0, len)
            Ok(arr)

    /// Removes the attribute from the given object.
    let RemoveAttribute (obj: NativeObject) (aid: NativeAttrID) =
        Extern.cps_api_object_attr_delete(obj, aid)
        Ok()

    /// Creates a new object list.
    let CreateObjectList () =
        let list = Extern.cps_api_object_list_create()
        if list <> IntPtr.Zero then
            Ok list
        else
            Error "Cannot create native object list."

    /// Destroys an object list.
    let DestroyObjectList (destroyObjects: bool) (list: NativeObjectList) =
        Extern.cps_api_object_list_destroy(list, destroyObjects)
        Ok()

    /// Appends an element to an object list.
    let AppendObjectToList list obj =
        if Extern.cps_api_object_list_append(list, obj) then
            Ok list
        else
            Error "Cannot append native object to list."

    /// Creates a new CPS Get request.
    let CreateGetRequest () =
        let req = NativeGetParams()
        let ret = Extern.cps_api_get_request_init(req)
        if ret = 0 then
            Ok req
        else
            Error (sprintf "Cannot inizialize native get request (Return value: %s = %d)." (ReturnValueToString ret) ret)

    /// Destroys the Get request.
    let DestroyGetRequest (req: NativeGetParams) =
        let ret = Extern.cps_api_get_request_close(req)
        if ret = 0 then
            Ok()
        else
            Error (sprintf "Error freeing get request (Return value: %s = %d)." (ReturnValueToString ret) ret)

    // Sends the given get request.
    let GetRequestSend (req: NativeGetParams) =
        let ret = Extern.cps_api_get(req)
        if ret = 0 then
            Ok()
        else
            Error (sprintf "Error sending get request (Return value: %s = %d)." (ReturnValueToString ret) ret)

    /// Creates a new CPS transaction.
    let CreateTransaction () =
        let trans = NativeTransactionParams()
        let ret = Extern.cps_api_transaction_init(trans)
        if ret = 0 then
            Ok trans
        else
            Error (sprintf "Cannot initialize native transaction (Return value: %s = %d)." (ReturnValueToString ret) ret)

    /// Releases the resources of a native CPS transaction.
    let DestroyTransaction (trans: NativeTransactionParams) =
        let ret = Extern.cps_api_transaction_close(trans)
        if ret = 0 then
            Ok()
        else
            Error (sprintf "Error freeing transaction (Return value: %s = %d)." (ReturnValueToString ret) ret)

    /// Adds a `create` operation to a transaction.
    let TransactionAddCreate (trans: NativeTransactionParams) (obj: NativeObject) =
        let ret = Extern.cps_api_create(trans, obj)
        if ret = 0 then
            Ok()
        else
            Error (sprintf "Error adding operation to transaction (Return value: %s = %d)." (ReturnValueToString ret) ret)

    /// Adds a `set` operation to a transaction.
    let TransactionAddSet (trans: NativeTransactionParams) (obj: NativeObject) =
        let ret = Extern.cps_api_set(trans, obj)
        if ret = 0 then
            Ok()
        else
            Error (sprintf "Error adding operation to transaction (Return value: %s = %d)." (ReturnValueToString ret) ret)

    /// Adds a `delete` operation to a transaction.
    let TransactionAddDelete (trans: NativeTransactionParams) (obj: NativeObject) =
        let ret = Extern.cps_api_delete(trans, obj)
        if ret = 0 then
            Ok()
        else
            Error (sprintf "Error adding operation to transaction (Return value: %s = %d)." (ReturnValueToString ret) ret)

    /// Commits a transaction.
    let TransactionCommit (trans: NativeTransactionParams) =
        let ret = Extern.cps_api_commit(trans)
        if ret = 0 then
            Ok()
        else
            Error (sprintf "Error committing transaction (Return value: %s = %d)." (ReturnValueToString ret) ret)