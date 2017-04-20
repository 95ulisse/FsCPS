﻿namespace FsCPS

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
open System.Runtime.InteropServices
open System.Text
open FsCPS


// Definition of native types.
// Most of them are just structures with a single field of type nativeint:
// this is just to mirror the fact the most of the types in the C library
// are just typedefs to void*. We could have used a simple nativeint anywhere,
// but distinct types help us keep a little bit of type safety.

[<Struct>]
[<StructLayout(LayoutKind.Sequential)>]
type internal NativeKey =
    [<MarshalAs(UnmanagedType.ByValArray, SizeConst = 256)>]
    val mutable raw: uint8[]


[<Struct>]
[<StructLayout(LayoutKind.Sequential)>]
type internal NativeObject =
    val mutable pointer: nativeint


[<Struct>]
[<StructLayout(LayoutKind.Sequential)>]
type internal NativeObjectAttr =
    val mutable pointer: nativeint


[<Struct>]
[<StructLayout(LayoutKind.Sequential)>]
type internal NativeObjectList =
    val mutable pointer: nativeint


[<Struct>]
[<StructLayout(LayoutKind.Sequential)>]
type internal NativeGetParams =
    val mutable keys: nativeint // Pointer to array of Key
    val mutable key_count: unativeint
    val mutable list: NativeObjectList
    val mutable filters: NativeObjectList
    val mutable timeout: unativeint


[<Struct>]
[<StructLayout(LayoutKind.Sequential)>]
type internal NativeTransactionParams =
    val mutable change_list: NativeObjectList
    val mutable prev: NativeObjectList
    val mutable timeout: unativeint


// Other aliases
type internal NativeAttrID = uint64


/// Definitions of all the native methods needed by FsCPS.
module internal NativeMethods =

    // Private module to make sure that the extern functions are not directly available to the outside
    module private Extern =

        [<Literal>]
        let objectLibrary = "libcps-api-common.so"

        // Library initialization

        [<DllImport(objectLibrary)>]
        extern void cps_api_class_map_init()

        // Keys, objects, attributes

        [<DllImport(objectLibrary, CharSet = CharSet.Ansi)>]
        extern NativeObject cps_api_object_create_int(string desc, uint32 line, string name)

        [<DllImport(objectLibrary)>]
        extern void cps_api_object_delete(NativeObject obj)

        [<DllImport(objectLibrary)>]
        extern nativeint (* NativeKey* *) cps_api_object_key(NativeObject obj)

        [<DllImport(objectLibrary)>]
        extern bool cps_api_key_from_attr_with_qual(NativeKey& key, NativeAttrID aid, CPSQualifier qual)

        [<DllImport(objectLibrary, EntryPoint = "cps_api_key_from_attr_with_qual")>]
        extern bool cps_api_key_from_attr_with_qual_ptr(nativeint key, NativeAttrID aid, CPSQualifier qual)

        [<DllImport(objectLibrary, CharSet = CharSet.Ansi)>]
        extern bool cps_class_string_to_key(string name, nativeint attrIds, unativeint& maxSize)

        [<DllImport(objectLibrary, CharSet = CharSet.Ansi)>]
        extern nativeint cps_api_key_print(nativeint key, StringBuilder buffer, unativeint len)

        [<DllImport(objectLibrary)>]
        extern bool cps_api_set_key_data(NativeObject obj, NativeAttrID id, CPSAttributeType atype, nativeint data, unativeint len)

        [<DllImport(objectLibrary)>]
        extern bool cps_api_object_e_add(
            NativeObject obj,
            [<MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2s)>] NativeAttrID[] aid,
            unativeint id_size,
            CPSAttributeType atype,
            nativeint data,
            unativeint len
        )

        [<DllImport(objectLibrary)>]
        extern void cps_api_object_attr_delete(NativeObject obj, NativeAttrID aid)

        [<DllImport(objectLibrary)>]
        extern NativeObject cps_api_object_list_get(NativeObjectList list, unativeint i)

        [<DllImport(objectLibrary)>]
        extern NativeObjectAttr cps_api_object_attr_get(NativeObject obj, NativeAttrID aid)

        [<DllImport(objectLibrary)>]
        extern uint16 cps_api_object_attr_data_u16(NativeObjectAttr attr)

        [<DllImport(objectLibrary)>]
        extern uint32 cps_api_object_attr_data_u32(NativeObjectAttr attr)

        [<DllImport(objectLibrary)>]
        extern uint64 cps_api_object_attr_data_u64(NativeObjectAttr attr)

        [<DllImport(objectLibrary)>]
        extern nativeint cps_api_object_attr_data_bin(NativeObjectAttr attr)

        // Transactions

        [<DllImport(objectLibrary)>]
        extern int cps_api_transaction_init(NativeTransactionParams& trans)

        [<DllImport(objectLibrary)>]
        extern int cps_api_transaction_close(NativeTransactionParams& trans)

        [<DllImport(objectLibrary)>]
        extern int cps_api_get(NativeGetParams& req)

        [<DllImport(objectLibrary)>]
        extern int cps_api_create(NativeTransactionParams& trans, NativeObject obj)

        [<DllImport(objectLibrary)>]
        extern int cps_api_set(NativeTransactionParams& trans, NativeObject obj)

        [<DllImport(objectLibrary)>]
        extern int cps_api_delete(NativeTransactionParams& trans, NativeObject obj)

        [<DllImport(objectLibrary)>]
        extern int cps_api_get_request_init(NativeGetParams& req)

        [<DllImport(objectLibrary)>]
        extern int cps_api_get_request_close(NativeGetParams& req)

        [<DllImport(objectLibrary)>]
        extern int cps_api_commit(NativeTransactionParams& trans)


    // Initializes the native library as soon as the module is loaded.
    do
        Extern.cps_api_class_map_init()

    /// Returns the string representation of the fiven string.
    let PrintKey key =
        let sb = StringBuilder(1024)
        Extern.cps_api_key_print(key, sb, 1024un) |> ignore
        sb.ToString()

    /// Converts a human-readable string path to an Attribute ID.
    let AttrIdFromPath path =
        
        // Alloc an array to hold the results
        let mutable len = 64un
        let arr = Array.zeroCreate<NativeAttrID> 64
        let handle = GCHandle.Alloc(arr, GCHandleType.Pinned)

        let result =  Extern.cps_class_string_to_key(path, handle.AddrOfPinnedObject(), &len)
        handle.Free()

        if result then
            Ok arr.[int len - 1]
        else
            Error ("Cannot find path " + path)

    /// Sets an object's key.
    let SetObjectKey qual path obj =
        let k = Extern.cps_api_object_key(obj)
        if k = IntPtr.Zero then
            Error "Could not get object's key."
        else
            AttrIdFromPath path
            >>= (fun attrId ->
                if Extern.cps_api_key_from_attr_with_qual_ptr(k, attrId, qual) then
                    Ok obj
                else
                    Error "Could not set object's key."
            )

    /// Creates a new CPS object.
    let CreateObject () =
        let obj = Extern.cps_api_object_create_int("", 0u, "")
        if obj.pointer = IntPtr.Zero then
            Error "Cannot create native CPS object"
        else
            Ok obj

    /// Frees the resources of a native CPS object.
    let DestroyObject (obj: NativeObject) =
        Extern.cps_api_object_delete(obj)

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

    /// Removes the attribute from the given object.
    let RemoveAttribute (obj: NativeObject) (aid: NativeAttrID) =
        Extern.cps_api_object_attr_delete(obj, aid)
        Ok()

    /// Creates a new CPS transaction.
    let CreateTransaction () =
        let mutable trans = NativeTransactionParams()
        if Extern.cps_api_transaction_init(&trans) = 0 then
            Ok trans
        else
            Error "Memory allocation error."

    /// Releases the resources of a native CPS transaction.
    let DestroyTransaction (trans: NativeTransactionParams) =
        let mutable t = trans
        if Extern.cps_api_transaction_close(&t) = 0 then
            Ok()
        else
            Error "Error freeing transaction."

    /// Adds a `create` operation to a transaction.
    let TransactionAddCreate (trans: NativeTransactionParams) (obj: NativeObject) =
        let mutable t = trans
        if Extern.cps_api_create(&t, obj) = 0 then
            Ok()
        else
            Error "Error adding operation to transaction."

    /// Adds a `set` operation to a transaction.
    let TransactionAddSet (trans: NativeTransactionParams) (obj: NativeObject) =
        let mutable t = trans
        if Extern.cps_api_set(&t, obj) = 0 then
            Ok()
        else
            Error "Error adding operation to transaction."

    /// Adds a `delete` operation to a transaction.
    let TransactionAddDelete (trans: NativeTransactionParams) (obj: NativeObject) =
        let mutable t = trans
        if Extern.cps_api_delete(&t, obj) = 0 then
            Ok()
        else
            Error "Error adding operation to transaction."

    /// Commits a transaction.
    let TransactionCommit (trans: NativeTransactionParams) =
        let mutable t = trans
        if Extern.cps_api_commit(&t) = 0 then
            Ok()
        else
            Error "Error committing transaction."