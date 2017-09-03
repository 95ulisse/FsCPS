[<AutoOpen>]
module FsCPS.Yang.ActivePatterns

open System
open System.Collections.Generic

let private allValues (t: YANGType) (prop: YANGTypeProperty<IList<_>>) =
    let rec allValues (t: YANGType) =
        seq {
            match t.GetProperty(prop) with
            | Some l -> yield! l
            | None -> ()
            match t.BaseType with
            | Some b -> yield! allValues (b.ResolvedType)
            | None -> ()
        }
    allValues t



// Some active patterns to help recognize yang primitive types

let (|Empty|_|) (t: YANGType) =
    match t.PrimitiveType with
    | x when x = YANGPrimitiveTypes.Empty -> Some Empty
    | _ -> None

let (|Boolean|_|) (t: YANGType) =
    match t.PrimitiveType with
    | x when x = YANGPrimitiveTypes.Boolean -> Some Boolean
    | _ -> None

let (|Int8|_|) (t: YANGType) =
    match t.PrimitiveType with
    | x when x = YANGPrimitiveTypes.Int8 -> Some Int8
    | _ -> None

let (|Int16|_|) (t: YANGType) =
    match t.PrimitiveType with
    | x when x = YANGPrimitiveTypes.Int16 -> Some Int16
    | _ -> None

let (|Int32|_|) (t: YANGType) =
    match t.PrimitiveType with
    | x when x = YANGPrimitiveTypes.Int32 -> Some Int32
    | _ -> None

let (|Int64|_|) (t: YANGType) =
    match t.PrimitiveType with
    | x when x = YANGPrimitiveTypes.Int64 -> Some Int64
    | _ -> None

let (|UInt8|_|) (t: YANGType) =
    match t.PrimitiveType with
    | x when x = YANGPrimitiveTypes.UInt8 -> Some UInt8
    | _ -> None

let (|UInt16|_|) (t: YANGType) =
    match t.PrimitiveType with
    | x when x = YANGPrimitiveTypes.UInt16 -> Some UInt16
    | _ -> None

let (|UInt32|_|) (t: YANGType) =
    match t.PrimitiveType with
    | x when x = YANGPrimitiveTypes.UInt32 -> Some UInt32
    | _ -> None

let (|UInt64|_|) (t: YANGType) =
    match t.PrimitiveType with
    | x when x = YANGPrimitiveTypes.UInt64 -> Some UInt64
    | _ -> None

let (|Decimal64|_|) (t: YANGType) =
    match t.PrimitiveType with
    | x when x = YANGPrimitiveTypes.Decimal64 -> Some Decimal64
    | _ -> None

let (|String|_|) (t: YANGType) =
    match t.PrimitiveType with
    | x when x = YANGPrimitiveTypes.String -> Some String
    | _ -> None

let (|Binary|_|) (t: YANGType) =
    match t.PrimitiveType with
    | x when x = YANGPrimitiveTypes.Binary -> Some Binary
    | _ -> None

let (|Enumeration|_|) (t: YANGType) =
    match t.PrimitiveType with
    | x when x = YANGPrimitiveTypes.Enumeration -> Some (allValues t YANGTypeProperties.EnumValues)
    | _ -> None

let (|Union|_|) (t: YANGType) =
    match t.PrimitiveType with
    | x when x = YANGPrimitiveTypes.Union -> Some (allValues t YANGTypeProperties.UnionMembers)
    | _ -> None