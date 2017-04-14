namespace FsCPS.Yang.Model

open System
open System.Collections.Generic
open System.Globalization
open System.Text.RegularExpressions
open FsCPS.Yang
open FsCPS.Yang.Statements


/// Base class for all YANG nodes
[<AbstractClass>]
[<AllowNullLiteral>]
type YANGNode() =
    member val OriginalStatement: Statement option = None with get, set


/// Base class for a YANG node that carries actual data.
[<AbstractClass>]
type YANGDataNode() =
    inherit YANGNode()


/// Status of a YANG schema node.
/// The default status for all nodes is `Current`.
/// Note that a node with status `Current` cannot reference any node with status
/// `Deprecated` or `Obsolete`, as well as a `Deprecated` node cannot reference
/// an `Obsolete` node.
type YANGStatus =
    | Current = 1
    | Deprecated = 2
    | Obsolete = 3


/// Ordering of a list.
/// With `System` the ordering of the elements in the list has no meaning,
/// while with `User`, the ordering of the children is important and must be respected.
/// See RFC 6020 section 7.7.1 for other details.
type YANGListOrderedBy =
    | System = 1
    | User = 2


/// Representation of all the possible errors that can happen
/// during the parsing and validation of a YANG model
[<StructuredFormatDisplay("{Text}")>]
type SchemaError =
    
    // Generic errors
    | ParserError of string
    | SchemaError of YANGNode * string
    
    // YANG Version
    | UnsupportedYangVersion of Statement * string
    
    // Statement argument
    | ArgumentExpected of Statement
    | NoArgumentExpected of Statement
    | ArgumentParserError of Statement * string
    
    // Statement cardinality
    | ExpectedStatement of Statement * string
    | UnexpectedStatement of Statement
    | MissingRequiredStatement of Statement * string
    | TooManyInstancesOfStatement of Statement

    // Namespaces and modules
    | AlreadyUsedModuleName of Statement * YANGModule
    | UnknownPrefix of Statement * string
    | AlreadyUsedPrefix of Statement * Statement
    | AlreadyUsedNamespace of Statement * YANGModule

    // Types
    | ShadowedType of Statement * YANGType
    | InvalidDefault of YANGType * YANGTypeRestriction
    | InvalidLeafDefault of YANGLeaf * YANGTypeRestriction
    | UnresolvedTypeRef of Statement * string
    | InvalidTypeRestriction of Statement * YANGType
    | DuplicateEnumName of YANGEnumValue
    | DuplicateEnumValue of YANGEnumValue

    with

        member this.Text = this.ToString()

        override this.ToString() =
            let (stmt, msg) =
                match this with
                | ParserError(x) ->
                    None, x
                | SchemaError(x, y) ->
                    x.OriginalStatement, y
                | UnsupportedYangVersion(x, y) ->
                    Some x, (sprintf "Unsupported YANG version %s." y)
                | ArgumentExpected(x) ->
                    Some x, (sprintf "Statement \"%s\" expects an argument." x.Name)
                | NoArgumentExpected(x) ->
                    Some x, (sprintf "Statement \"%s\" does not expect an argument." x.Name)
                | ArgumentParserError(x, y) ->
                    Some x, (sprintf "Error parsing argument for statement \"%s\":%s%s" x.Name Environment.NewLine y)
                | ExpectedStatement(x, y) ->
                    Some x, (sprintf "Expected \"%s\" statement, but got \"%s\"." x.Name y)
                | UnexpectedStatement(x) ->
                    Some x, (sprintf "Unexpected statement \"%s\"." x.Name)
                | MissingRequiredStatement(x, y) ->
                    Some x, (sprintf "Missing required statement \"%s\"." y)
                | TooManyInstancesOfStatement(x) ->
                    Some x, (sprintf "Too many instances of the \"%s\" statement." x.Name)
                | AlreadyUsedModuleName(x, y) ->
                    let stmt: Statement option = y.OriginalStatement
                    match stmt with
                    | Some(s) -> Some x, (sprintf "Module name already used by module at %A." s.Position)
                    | None -> Some x, "Module name already used."
                | UnknownPrefix(x, y) ->
                    Some x, (sprintf "Unknown prefix \"%s\"." y)
                | AlreadyUsedPrefix(x, y) ->
                    Some x, (sprintf "Prefix already registered. See statement at %d:%d" y.Position.Line y.Position.Column)
                | AlreadyUsedNamespace(x, y) ->
                    let stmt: Statement option = y.OriginalStatement
                    match stmt with
                    | Some(s) -> Some x, (sprintf "Namespace already registered by module \"%A\" (%A)." y.Name s.Position)
                    | None -> Some x, (sprintf "Namespace already registered by module \"%A\"." y.Name)
                | ShadowedType(x, y) ->
                    let stmt: Statement option = y.OriginalStatement
                    match stmt with
                    | Some(s) -> Some x, (sprintf "This type shadows the type %A defined at %A." y.Name s.Position)
                    | None -> Some x, "This type shadows a type defined in an higher scope."
                | InvalidDefault(x, y) ->
                    let restriction =
                        match y.OriginalStatement with
                        | Some(s) -> s.Position.ToString()
                        | None -> "<position not available>"
                    x.OriginalStatement, (sprintf "This type has an invalid default value. See restriction at %s." restriction)
                | InvalidLeafDefault(x, y) ->
                    let restriction =
                        match y.OriginalStatement with
                        | Some(s) -> s.Position.ToString()
                        | None -> "<position not available>"
                    x.OriginalStatement, (sprintf "This leaf has an invalid default value. See restriction at %s." restriction)
                | UnresolvedTypeRef(x, y) ->
                    Some x, (sprintf "Cannot find type %s." y)
                | InvalidTypeRestriction(x, y) ->
                    Some(x), (sprintf "This restriction cannot be applied to the type %A." y.Name)
                | DuplicateEnumName(x) ->
                    x.OriginalStatement, (sprintf "The enum name \"%s\" has already been used." x.Name)
                | DuplicateEnumValue(x) ->
                    x.OriginalStatement, (sprintf "The enum value \"%d\" has already been used." x.Value.Value)
            
            match stmt with
            | Some(s) -> sprintf "Statement \"%s\" (%A): %s" s.Name s.Position msg
            | None -> msg


/// Namespace used in a YANG model.
/// Namespaces are strictly tied to the module that defined them.
and [<StructuredFormatDisplay("{Uri}")>] YANGNamespace =
    {
        Module: YANGModule;
        Uri: Uri;
    }
    with
        static member Default = { Module = null; Uri = Uri("urn:ietf:params:xml:ns:yang:1") }
        static member Invalid = { Module = null; Uri = null }


/// Name of YANGNode.
/// Names can't be simple strings because they need to be qualified by a namespace.
and [<StructuredFormatDisplay("{Namespace}{Name}")>] YANGName =
    {
        Namespace: YANGNamespace;
        Name: string;
    }



// -------------------------------------------------------------------
// Implementation of all the supported YANG data types
// -------------------------------------------------------------------

/// Restriction on the value of a YANG type.
and [<AbstractClass>] YANGTypeRestriction() =
    inherit YANGNode()

    member val ErrorMessage: string = null with get, set
    member val ErrorAppTag: string = null with get, set
    member val Description: string = null with get, set
    member val Reference: string = null with get, set

    abstract member IsValid: obj -> bool


/// Restriction on the value of a numeral type.
and YANGRangeRestriction(ranges: Range<float> list) =
    inherit YANGTypeRestriction()

    override this.IsValid o =
        if isNull o then
            false
        else
            try
                let n = Convert.ToDouble(o)
                ranges
                |> List.exists (fun range ->
                    n >= range.Min && n <= range.Max
                )
            with
            | _ -> false


/// Restriction on the length of a string or binary data.
and YANGLengthRestriction(ranges: Range<uint32> list) =
    inherit YANGTypeRestriction()

    override __.IsValid o =
        let len =
            if isNull o then
                None
            else if o :? string then
                let str = o :?> string
                Some(uint32 str.Length)
            else if o :? byte[] then
                let arr = o :?> byte[]
                Some(uint32 arr.Length)
            else
                None
        match len with
        | None -> false
        | Some(n) ->
            ranges
            |> List.exists (fun range ->
                n >= range.Min && n <= range.Max
            )


/// Restricts a string value to a given pattern.
and YANGPatternRestriction(r: Regex) =
    inherit YANGTypeRestriction()

    member __.Pattern = r

    override __.IsValid o =
        if not (isNull o) && o :? string then
            r.IsMatch(o :?> string)
        else
            false


/// One possible value for a YANG enum type.
and YANGEnumValue(name: string) =
    inherit YANGNode()

    member val Name = name
    member val Description: string = null with get, set
    member val Reference: string = null with get, set
    member val Status = YANGStatus.Current with get, set
    member val Value: int option = None with get, set


/// Base class for all YANG data types.
/// The properties `Default`, `Description`, `Reference`, `Status` and `Units`
/// inherit thei values from the `BaseType` if not set manually.
and YANGType(name: YANGName) =
    inherit YANGNode()

    let mutable _default: obj option = None
    let mutable _description: string option = None
    let mutable _reference: string option = None
    let mutable _status: YANGStatus option = None
    let mutable _units: string option = None
    let _properties = Dictionary<string, obj>()

    member val Name = name
    
    member val BaseType: YANGType option = None with get, set
    
    member this.Default
        with get() =
            match _default, this.BaseType with
            | None, Some b -> b.Default
            | None, None -> null
            | Some v, _ -> v
        and set(v) =
            _default <- Some v

    member this.Description
        with get() =
            match _description, this.BaseType with
            | None, Some b -> b.Description
            | None, None -> null
            | Some v, _ -> v
        and set(v) =
            _description <- Some v
    
    member this.Reference
        with get() =
            match _reference, this.BaseType with
            | None, Some b -> b.Reference
            | None, None -> null
            | Some v, _ -> v
        and set(v) =
            _reference <- Some v
    
    member this.Status
        with get() =
            match _status, this.BaseType with
            | None, Some b -> b.Status
            | None, None -> YANGStatus.Current
            | Some v, _ -> v
        and set(v) =
            _status <- Some v
    
    member this.Units
        with get() =
            match _units, this.BaseType with
            | None, Some b -> b.Units
            | None, None -> null
            | Some v, _ -> v
        and set(v) =
            _units <- Some v

    member val Restrictions = ResizeArray<YANGTypeRestriction>()

    member this.SetProperty(name, value: #obj) =
        _properties.Add(name, value)

    member this.GetProperty<'T>(name) =
        match _properties.TryGetValue(name), this.BaseType with
        | (true, value), _ -> Some(value :?> 'T)
        | _, Some(b) -> b.GetProperty(name)
        | _, None -> None

    abstract member CanBeRestrictedWith: YANGTypeRestriction -> bool
    default this.CanBeRestrictedWith(r: YANGTypeRestriction) =
        match this.BaseType with
        | Some(b) -> b.CanBeRestrictedWith(r)
        | None -> invalidOp (sprintf "Missing restriction filter logic for type %A" this.Name)

    member this.IsValid o =
        let violatedRestriction =
            this.Restrictions
            |> Seq.tryFind (fun r -> not (r.IsValid o))
        match violatedRestriction, this.BaseType with
        | Some r, _ -> Error r
        | None, Some b -> b.IsValid o
        | None, None -> Ok()
    
    member this.Parse str =
        this.ParseCore(this, str)

    member this.Serialize o =
        this.SerializeCore(this, o)

    member this.CheckRequiredProperties() =
        this.CheckRequiredPropertiesCore(this)

    abstract member ParseCore: YANGType * string -> obj option
    default this.ParseCore(actualType: YANGType, str: string) =
        match this.BaseType with
        | Some(b) -> b.ParseCore(actualType, str)
        | None -> invalidOp (sprintf "Missing parsing logic for type %A" this.Name)

    abstract member SerializeCore: YANGType * obj -> string option
    default this.SerializeCore(actualType: YANGType, o: obj) =
        match this.BaseType with
        | Some(b) -> b.SerializeCore(actualType, o)
        | None -> invalidOp (sprintf "Missing serializing logic for type %A" this.Name)

    abstract member CheckRequiredPropertiesCore: YANGType -> Result<unit, SchemaError list>
    default this.CheckRequiredPropertiesCore(actualType) =
        match this.BaseType with
        | Some(b) -> b.CheckRequiredPropertiesCore(actualType)
        | None -> Ok()


/// Static class with all the primitive types defined by YANG.
and YANGPrimitiveTypes private () =

    static member internal AllEnumValues(t: YANGType) =
        let rec allValues (t: YANGType) =
            seq {
                match t.GetProperty<IList<YANGEnumValue>>("enum-values") with
                | Some l -> yield! l
                | None -> ()
                match t.BaseType with
                | Some b -> yield! allValues b
                | None -> ()
            }
        allValues t
    
    static member private MakeIntegralType<'T> name (tryParse: string -> bool * 'T) =

        {
            new YANGType({ Namespace = YANGNamespace.Default; Name = name }) with

                override this.ParseCore(_, str) =
                    if isNull str then
                        None
                    else
                        match tryParse str with
                        | (true, obj) -> Some(box obj)
                        | _ -> None

                override this.SerializeCore(_, o) =
                    if isNull o || not (o :? 'T)then
                        None
                    else
                        Some(o.ToString())

                override this.CanBeRestrictedWith r =
                    r :? YANGRangeRestriction

        }

    static member Empty = {
        new YANGType({ Namespace = YANGNamespace.Default; Name = "empty" }) with

            override this.ParseCore(_, _) =
                None

            override this.SerializeCore(_, _) =
                None

            override this.CanBeRestrictedWith r =
                false

    }

    static member Boolean = {
        new YANGType({ Namespace = YANGNamespace.Default; Name = "boolean" }) with

            override this.ParseCore(_, str) =
                if isNull str then
                    None
                else if str = "true" then
                    Some(box true)
                else if str = "false" then
                    Some(box false)
                else
                    None

            override this.SerializeCore(_, o) =
                if isNull o || not (o :? bool) then
                    None
                else
                    Some(if o :?> bool then "true" else "false")

            override this.CanBeRestrictedWith r =
                false

    }

    static member Int8 = YANGPrimitiveTypes.MakeIntegralType "int8" System.SByte.TryParse
    static member Int16 = YANGPrimitiveTypes.MakeIntegralType "int16" System.Int16.TryParse
    static member Int32 = YANGPrimitiveTypes.MakeIntegralType "int32" System.Int32.TryParse
    static member Int64 = YANGPrimitiveTypes.MakeIntegralType "int64" System.Int64.TryParse
    static member UInt8 = YANGPrimitiveTypes.MakeIntegralType "uint8" System.Byte.TryParse
    static member UInt16 = YANGPrimitiveTypes.MakeIntegralType "uint16" System.UInt16.TryParse
    static member UInt32 = YANGPrimitiveTypes.MakeIntegralType "uint32" System.UInt32.TryParse
    static member UInt64 = YANGPrimitiveTypes.MakeIntegralType "uint64" System.UInt64.TryParse

    static member Decimal64 = {
        new YANGType({ Namespace = YANGNamespace.Default; Name = "decimal64" }) with

            override this.ParseCore(actualType, str) =
                let digits =
                    match actualType.GetProperty<int>("fraction-digits") with
                    | Some(d) when d >= 1 && d <= 18 -> d
                    | _ -> invalidArg "fraction-digits" "Invalid value for required property \"fraction-digits\": allowed values are from 0 to 18."

                if isNull str then
                    None
                else
                
                    let min = (float Int64.MinValue) / (10.0 ** float digits)
                    let max = (float Int64.MaxValue) / (10.0 ** float digits)
                    
                    // A decimal64 number is just a signed 64 bit integer with a decimal point in the middle.
                    // This means that we can parse it like any other ordinary floating point number,
                    // and then we just check that it is between Int64.MinValue * 10^(-digits)
                    // and Int64.MaxValue * 10^(-digits).
                    // We also need to check that contains at least one decimal digit, since integers are not allowed,
                    // and TryParse would parse them.

                    let dotIndex = str.IndexOf('.')
                    if dotIndex < 1 || dotIndex > str.Length - 2 then
                        None
                    else

                        let numberStyle =
                            NumberStyles.AllowDecimalPoint |||
                            NumberStyles.AllowLeadingWhite |||
                            NumberStyles.AllowTrailingWhite |||
                            NumberStyles.AllowLeadingSign
                        match System.Double.TryParse(str, numberStyle, NumberFormatInfo.InvariantInfo) with
                        | (true, n) ->
                            if (not (Double.IsNaN n)) && n >= min && n <= max then
                                Some(box n)
                            else
                                None
                        | _ -> None

            override this.SerializeCore(actualType, o) =
                let digits =
                    match actualType.GetProperty<int>("fraction-digits") with
                    | Some(d) when d >= 1 && d <= 18 -> d
                    | _ -> invalidArg "fraction-digits" "Invalid value for required property \"fraction-digits\": allowed values are from 0 to 18."
                
                if o :? float then
                    let n = o :?> float
                    let min = (float Int64.MinValue) / (10.0 ** float digits)
                    let max = (float Int64.MaxValue) / (10.0 ** float digits)

                    if Double.IsNaN(n) || n < min || n > max then
                        None
                    else
                        let formatString = sprintf "0.%s" (String('0', digits))
                        Some (n.ToString(formatString, NumberFormatInfo.InvariantInfo))
                else
                    None

            override this.CanBeRestrictedWith r =
                r :? YANGRangeRestriction

            override this.CheckRequiredPropertiesCore(actualType) =
                match actualType.GetProperty<int>("fraction-digits") with
                    | Some(d) when d >= 1 && d <= 18 -> Ok()
                    | Some _ -> Error([ ArgumentParserError(actualType.OriginalStatement.Value, "Invalid value for required property \"fraction-digits\": allowed values are from 0 to 18.") ])
                    | None -> Error([ MissingRequiredStatement(actualType.OriginalStatement.Value, "fraction-digits") ])

    }

    static member String = {
        new YANGType({ Namespace = YANGNamespace.Default; Name = "string" }) with

            override this.ParseCore(_, str) =
                if isNull str then
                    None
                else
                    Some(str :> obj)

            override this.SerializeCore(_, o) =
                if isNull o || not (o :? string) then
                    None
                else
                    Some(o :?> string)

            override this.CanBeRestrictedWith r =
                r :? YANGLengthRestriction || r :? YANGPatternRestriction

    }

    static member Binary = {
        new YANGType({ Namespace = YANGNamespace.Default; Name = "string" }) with

            override this.ParseCore(_, str) =
                if isNull str then 
                    None
                else
                    try
                        Some(Convert.FromBase64String(str) :> obj)
                    with
                    | :? FormatException -> None

            override this.SerializeCore(_, o) =
                if isNull o || not (o :? byte[]) then
                    None
                else
                    Some(Convert.ToBase64String(o :?> byte[]))

            override this.CanBeRestrictedWith r =
                r :? YANGLengthRestriction

    }

    static member Enumeration = {
        new YANGType({ Namespace = YANGNamespace.Default; Name = "enumeration" }) with

            override this.ParseCore(actualType, str) =
                if isNull str then
                    None
                else
                    YANGPrimitiveTypes.AllEnumValues(actualType)
                    |> Seq.tryPick (fun v ->
                        if v.Name = str then
                            Some(v :> obj)
                        else
                            None
                    )

            override this.SerializeCore(actualType, value) =
                let chooser: (YANGEnumValue -> string option) option =
                    if isNull value then
                        None
                    else if value :? string then
                        Some (
                            fun v ->
                                if v.Name = (value :?> string) then
                                    Some(value :?> string)
                                else
                                    None
                        )
                    else if value :? int then
                        Some(
                            fun v ->
                                if v.Value.Value = (value :?> int) then
                                    Some(v.Name)
                                else
                                    None
                        )
                    else if value :? YANGEnumValue then
                        Some(
                            fun v ->
                                if v = (value :?> YANGEnumValue) then
                                    Some(v.Name)
                                else
                                    None
                        )
                    else
                        None
                
                chooser
                |> Option.bind (fun chooser ->
                    YANGPrimitiveTypes.AllEnumValues(actualType)
                    |> Seq.tryPick chooser
                )

            override this.CanBeRestrictedWith _ =
                false

            override this.CheckRequiredPropertiesCore(actualType) =
                match actualType.GetProperty<IList<YANGEnumValue>>("enum-values") with
                | Some l when l.Count > 0 -> Ok()
                | _ -> Error([ MissingRequiredStatement(actualType.OriginalStatement.Value, "enum") ])

    }

    static member FromName(name: string) =
        match name with
        | "empty" -> Some(YANGPrimitiveTypes.Empty)
        | "boolean" -> Some(YANGPrimitiveTypes.Boolean)
        | "int8" -> Some(YANGPrimitiveTypes.Int8)
        | "int16" -> Some(YANGPrimitiveTypes.Int16)
        | "int32" -> Some(YANGPrimitiveTypes.Int32)
        | "int64" -> Some(YANGPrimitiveTypes.Int64)
        | "uint8" -> Some(YANGPrimitiveTypes.UInt8)
        | "uint16" -> Some(YANGPrimitiveTypes.UInt16)
        | "uint32" -> Some(YANGPrimitiveTypes.UInt32)
        | "uint64" -> Some(YANGPrimitiveTypes.UInt64)
        | "string" -> Some(YANGPrimitiveTypes.String)
        | "binary" -> Some(YANGPrimitiveTypes.Binary)
        | "decimal64" -> Some(YANGPrimitiveTypes.Decimal64)
        | "enumeration" -> Some(YANGPrimitiveTypes.Enumeration)
        | _ -> None
        



// -------------------------------------------------------------------
// Implementation of all the supported nodes
// -------------------------------------------------------------------

and [<AllowNullLiteral>] YANGModule(unqualifiedName: string) =
    inherit YANGNode()
    
    member this.Name
        with get() =
            if this.Namespace = YANGNamespace.Invalid then
                invalidOp "Cannot retrive fully qualified name before setting the `Namespace` property."
            {
                Namespace = this.Namespace;
                Name = unqualifiedName;
            }

    member val Namespace = YANGNamespace.Invalid with get, set
    member val Prefix: string = null with get, set
    member val Contact: string = null with get, set
    member val Organization: string = null with get, set
    member val Description: string = null with get, set
    member val Reference: string = null with get, set

    member val Revisions = ResizeArray<YANGModuleRevision>()

    member val DataNodes = ResizeArray<YANGDataNode>()

    member val ExportedTypes = Dictionary<YANGName, YANGType>()


and YANGModuleRevision(date: DateTime) =
    inherit YANGNode()

    member val Date = date
    member val Description: string = null with get, set
    member val Reference: string = null with get, set


and YANGContainer(name: YANGName) =
    inherit YANGDataNode()

    member this.Name = name
    member val Presence: string = null with get, set
    member val Description: string = null with get, set
    member val Reference: string = null with get, set
    member val Status = YANGStatus.Current with get, set
    member val DataNodes = ResizeArray<YANGDataNode>()
    

and YANGLeaf(name: YANGName) =
    inherit YANGDataNode()

    member this.Name = name
    member val Type: YANGType = Unchecked.defaultof<YANGType> with get, set
    member val Units: string = null with get, set
    member val Default: obj = null with get, set
    member val Mandatory: bool = false with get, set
    member val Description: string = null with get, set
    member val Reference: string = null with get, set
    member val Status = YANGStatus.Current with get, set


and YANGLeafList(name: YANGName) =
    inherit YANGDataNode()

    member this.Name = name
    member val Description: string = null with get, set
    member val MaxElements = Int32.MaxValue with get, set
    member val MinElements = 0 with get, set
    member val OrderedBy = YANGListOrderedBy.System with get, set
    member val Reference: string = null with get, set
    member val Status = YANGStatus.Current with get, set
    member val Type = Unchecked.defaultof<YANGType> with get, set
    member val Units: string = null with get, set


and YANGList(name: YANGName) =
    inherit YANGDataNode()

    member this.Name = name
    member val Description: string = null with get, set
    member val MinElements = 0 with get, set
    member val MaxElements = Int32.MaxValue with get, set
    member val OrderedBy = YANGListOrderedBy.System with get, set
    member val Reference: string = null with get, set
    member val Status = YANGStatus.Current with get, set
    member val Unique = ResizeArray<string>()
    member val DataNodes = ResizeArray<YANGDataNode>()