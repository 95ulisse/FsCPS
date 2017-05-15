namespace FsCPS.Yang

open System
open System.Collections.Generic
open System.Globalization
open System.IO
open System.Reflection
open System.Resources
open System.Text
open System.Text.RegularExpressions
open FsCPS

[<AbstractClass; Sealed>]
type YANGParser private () =

    // Loads (via reflection) the private property `rootParser` which is added at the end of the file
    // `SchemaParsers.fs`. This is a HORRIBLE solution to the big circular dependency between all the types.
    static let rootParser: SchemaParser<YANGModule> =
        let t = typeof<YANGParser>.Assembly.GetType("FsCPS.Yang.SchemaParsers")
        let prop = t.GetProperty("rootParser", BindingFlags.Static ||| BindingFlags.NonPublic)
        prop.GetMethod.Invoke(null, null) :?> SchemaParser<YANGModule>

    static member ParseModule(str: string) =
        YANGParser.ParseModule(str, YANGParserOptions())

    static member ParseModule(stream: Stream) =
        YANGParser.ParseModule(stream, YANGParserOptions())

    static member ParseModule(str: string, options) =

        // Parses the string as a tree of statements
        match FParsec.CharParsers.run StatementParsers.root str with
        | FParsec.CharParsers.Failure(e, _, _) -> Error([ ParserError(e) ])
        | FParsec.CharParsers.Success(statement, _, _) ->

            // Transforms the statements to the actual schema tree
            let ctx = SchemaParserContext(statement, options)
            rootParser ctx

            // Rescan the AST to check for its consistency
            >>= (fun m -> m.EnsureRefs() |>> (fun _ -> m))

    static member ParseModule(stream: Stream, options) =

        // Parses the string as a tree of statements
        match FParsec.CharParsers.runParserOnStream StatementParsers.root () "" stream Encoding.UTF8 with
        | FParsec.CharParsers.Failure(e, _, _) -> Error([ ParserError(e) ])
        | FParsec.CharParsers.Success(statement, _, _) ->

            // Transforms the statements to the actual schema tree
            let ctx = SchemaParserContext(statement, options)
            rootParser ctx

            // Rescan the AST to check for its consistency
            >>= (fun m -> m.EnsureRefs() |>> (fun _ -> m))


/// Options for the YANG parser.
and YANGParserOptions() as this =

    static let builtinModules = [
        "ietf-yang-types";
        "ietf-inet-types"
    ]

    let moduleCache = Dictionary<(string * DateTime option), YANGModule>()

    let defaultResolveImport name revision =
        
        let checkBuiltin _ =
            if builtinModules |> List.exists (fun x -> x = name) then
                let resman = ResourceManager("FsCPS.g", Assembly.GetExecutingAssembly())
                use stream = resman.GetStream(sprintf "yang/models/%s.yang" (name.ToLowerInvariant()))
                YANGParser.ParseModule(stream, this)
            else
                Error([])

        let findFile _ =
            // Possible file paths
            let paths =
                [
                    match revision with
                    | Some d -> yield String.Format("{0}@{1:yyyy-MM-dd}.yang", name, d)
                    | None -> ()
                    yield sprintf "%s.yang" name
                ]
                |> List.map Path.GetFullPath

            // Check if one of the paths exists
            match paths |> List.tryFind File.Exists with
            | None -> Error([ CannotFindModule(name, paths) ])
            | Some modulePath ->

                // Parse the file contents
                try
                    use stream = File.OpenRead modulePath
                    YANGParser.ParseModule(stream, this)
                with
                | :? IOException as e -> Error([ CannotOpenModule(modulePath, e.Message) ])

        let storeModuleInCache (m: YANGModule) =
            let checkCachedAndReturn() =
                // We successfully parsed the module, but before returning it,
                // we have to check that a module with the same name and revision is not in the cache.
                // The check needs to be done here again because if the user passed no revision,
                // but the module has already been parsed, we need to return the cached module.
                let revisionDate = m.Revision |> Option.map (fun x -> x.Date)
                match moduleCache.TryGetValue((name, revisionDate)) with
                | (true, cachedModule) -> Ok cachedModule
                | _ ->
                    moduleCache.Add((name, revisionDate), m)
                    Ok m

            // Check that the parsed module has the same revision date if one has been
            // requested explicitly
            if revision.IsSome then
                if m.Revision.IsSome && revision.Value = m.Revision.Value.Date then
                    checkCachedAndReturn()
                else
                    Error([ CannotFindModuleRevision(name, revision.Value) ])
            else
                checkCachedAndReturn()

        // If a revision is provided, check if the module is in the cache
        match moduleCache.TryGetValue((name, revision)) with
        | (true, m) -> Ok m
        | _ -> Error()
        
        // Otherwise check if it's one of the builtin modules
        |> Result.bindError checkBuiltin

        // Start looking for a file
        |> Result.bindError (fun errors ->
            match errors with
            | [] -> findFile()
            | _ -> Error(errors)
        )

        // If a module has been found and parsed, check that the requested revision (if any) matches,
        // and then store the module in the cache.
        |> Result.bind storeModuleInCache

    member val ResolveImport: string -> DateTime option -> SchemaParserResult<YANGModule> = defaultResolveImport with get, set


/// Scope of a single statement.
/// A scope lives only between the children of a statement,
/// or in other words, a scope begins with a "{" and ends with a "}".
and internal Scope =
    {
        Module: YANGModule;
        ParentStatement: Statement;
        ParentNode: YANGNode;
        Prefixes: Dictionary<string, YANGModule>;
        Types: Dictionary<YANGName, YANGType>;
        Parent: Scope option;
    }
    with

        /// Returns the type with the given name.
        member this.GetType(name: YANGName) =
            let rec f scope =
                match scope.Types.TryGetValue(name), scope.Parent with
                | (true, t), _ -> Some(t)
                | _, Some parent -> f parent
                | _, None -> None
            f this

        /// Returns the module registered with the given prefix in the current scope.
        member this.GetIncludedModule(prefix: string) =
            let rec f scope =
                match scope.Prefixes.TryGetValue(prefix), scope.Parent with
                | (true, m), _ -> Some(m)
                | _, Some parent -> f parent
                | _, None -> None
            f this

        /// Registers a new type in the current scope.
        /// Returns an error if the new type would shadow an already defined one.
        member this.RegisterType(t: YANGType) =
            match YANGPrimitiveTypes.FromName(t.Name.Name) with
            | Some(shadowedType) -> Error([ ShadowedType(t.OriginalStatement.Value, shadowedType) ])
            | None ->
                match this.GetType(t.Name) with
                | Some(shadowedType) ->
                    Error([ ShadowedType(t.OriginalStatement.Value, shadowedType) ])
                | None ->
                    this.Types.Add(t.Name, t)
                    Ok()

        /// Resolves a reference to a type.
        member this.ResolveTypeRef(prefix: string option, name: string) =
            match prefix, YANGPrimitiveTypes.FromName(name) with
        
            // Reference to a primitive type
            | None, Some(primitiveType) ->
                Some(primitiveType)

            // We can either have an unprefixed reference, or a prefixed reference that is using
            // the same prefix of the module we are parsing. In both cases, resolve from the current scope.
            | None, None ->
                this.GetType({ Namespace = this.Module.Namespace; Name = name })
            | Some(p), _ when p = this.Module.Prefix ->
                this.GetType({ Namespace = this.Module.Namespace; Name = name })

            // We have a prefixed reference, so resolve the module associated to the prefix,
            // then search between it's exported types
            | Some(p), _ ->
                match this.GetIncludedModule(p) with
                | Some m ->
                    let fullName = { Namespace = m.Namespace; Name = name }
                    match (m.ExportedTypes :> Dictionary<_,_>).TryGetValue(fullName) with
                    | (true, t) -> Some(t)
                    | _ -> None
                | None -> None

        /// Registers the given module as available under the given prefix.
        member this.RegisterImportedModule(m: YANGModule, prefix: string) =
            match this.GetIncludedModule(prefix) with
            | Some oldModule ->
                Error([ AlreadyUsedPrefix(m.OriginalStatement.Value, oldModule.OriginalStatement.Value) ])
            | None ->
                this.Prefixes.Add(prefix, m)
                Ok()

/// Context of the parsing operation.
/// Contains a pointer to the current statement and other useful informations.
/// It also implements a stack-like semantics for scopes to aid recursion.
and internal SchemaParserContext(rootStatement: Statement, options: YANGParserOptions) =

    let _scopeStack = Stack<Scope>()

    /// Parsing options
    member val Options = options

    /// Root statement from which the parsing started.
    member val RootStatement: Statement = rootStatement

    /// Current statement that is being parsed.
    member val Statement = rootStatement with get, set

    /// Current scope.
    member this.Scope
        with get() = _scopeStack.Peek()

    /// Pushes a new scope with the given parent statements and node to the stack.
    member this.PushScope(parentStatement, parentNode: YANGNode) =
        if parentNode :? YANGModule then
            _scopeStack.Push(
                {
                    Module = parentNode :?> YANGModule;
                    ParentStatement = parentStatement;
                    ParentNode = parentNode;
                    Prefixes = Dictionary<_, _>();
                    Types = Dictionary<_, _>();
                    Parent = if _scopeStack.Count = 0 then None else Some (_scopeStack.Peek())
                }
            )
        else
            _scopeStack.Push(
                {
                    this.Scope with
                        ParentStatement = parentStatement;
                        ParentNode = parentNode;
                        Prefixes = Dictionary<_, _>();
                        Types = Dictionary<_, _>();
                        Parent = Some this.Scope
                }
            )

    /// Pops the last scope from the stack.
    member __.PopScope() =
        _scopeStack.Pop()

    /// Resolves an import and compiles the referenced module.
    member this.ResolveImport(name: string, revision: DateTime option) =
        this.Options.ResolveImport name revision


/// Result of a parser.
/// It can either be an error or a success, in which case the parsed element is returned.
and internal SchemaParserResult<'T> = Result<'T, SchemaError list>


/// A parser is a function from a parsing context to a result or an error list.
and internal SchemaParser<'T> = SchemaParserContext -> SchemaParserResult<'T>


/// Base class for all YANG nodes
and [<AbstractClass; AllowNullLiteral>] YANGNode() =
    member val OriginalStatement: Statement option = None with get, set
    abstract member EnsureRefs: unit -> SchemaParserResult<unit>


/// Base class for a YANG node that carries actual data.
and [<AbstractClass>] YANGDataNode() =
    inherit YANGNode()


/// Status of a YANG schema node.
/// The default status for all nodes is `Current`.
/// Note that a node with status `Current` cannot reference any node with status
/// `Deprecated` or `Obsolete`, as well as a `Deprecated` node cannot reference
/// an `Obsolete` node.
and YANGStatus =
    | Current = 1
    | Deprecated = 2
    | Obsolete = 3


/// Ordering of a list.
/// With `System` the ordering of the elements in the list has no meaning,
/// while with `User`, the ordering of the children is important and must be respected.
/// See RFC 6020 section 7.7.1 for other details.
and YANGListOrderedBy =
    | System = 1
    | User = 2


/// Representation of all the possible errors that can happen
/// during the parsing and validation of a YANG model
and [<StructuredFormatDisplay("{Text}")>] SchemaError =
    
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
    | CannotFindModule of string * seq<string>
    | CannotOpenModule of string * string
    | CannotFindModuleRevision of string * DateTime

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
                | CannotFindModule(x, y) ->
                    None, (sprintf "Unable to find imported module \"%s\". Searched paths: %s" x (String.Join(Environment.NewLine, y)))
                | CannotOpenModule(x, y) ->
                    None, (sprintf "Unable to open module \"%s\": %s" x y)
                | CannotFindModuleRevision(x, y) ->
                    None, (sprintf "Unable to find imported module \"%s\" with revision %s. Searched paths: %s" x (y.ToString("yyyy-MM-dd")) (String.Join(Environment.NewLine, y)))
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
and [<StructuredFormatDisplay("{Namespace} / {Name}")>] YANGName =
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

    override this.EnsureRefs() =
        Ok()


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


/// Option that can be set on a YANG type.
and YANGTypeProperty<'T> internal (name: string) =
    member val Name = name


/// Static class containing all the known options for YANG types.
and [<AbstractClass; Sealed>] YANGTypeProperties private () =
    static member val FractionDigits = YANGTypeProperty<int>("fraction-digits")
    static member val EnumValues = YANGTypeProperty<IList<YANGEnumValue>>("enum-values")
    static member val UnionMembers = YANGTypeProperty<IList<YANGTypeRef>>("union-members")


/// One possible value for a YANG enum type.
and YANGEnumValue(name: string) =
    inherit YANGNode()

    member val Name = name
    member val Description: string = null with get, set
    member val Reference: string = null with get, set
    member val Status = YANGStatus.Current with get, set
    member val Value: int option = None with get, set

    override this.EnsureRefs() =
        Ok()


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

    // Checks that the enum values for the given type are valid: checks the uniqueness of the name
    // and of the value. If the value has not been provided, it automatically computes
    // and assignes the next one.
    let rec checkEnumValuesForType (t: YANGType) : SchemaParserResult<int option * YANGEnumValue list> =

        // Checks the base type
        match t.BaseType with
        | None -> Ok (None, [])
        | Some b -> checkEnumValuesForType (b.ResolvedType)

        >>= (fun (maxValue, baseValues) ->

            let currentValues: YANGEnumValue list =
                match t.GetProperty(YANGTypeProperties.EnumValues, true) with
                | Some l -> l |> Seq.toList
                | _ -> List.empty

            let mutable newMaxValue = maxValue
            let mutable allValues = baseValues

            // Checks that the new values have unique names and values
            currentValues |> foldResult (fun _ v ->
                
                // Checks that both the name and the value are unique
                allValues
                |> foldResult (fun _ oldValue ->
                    if v.Name = oldValue.Name then
                        Error([ DuplicateEnumName(v) ])
                    else if v.Value.IsSome && v.Value.Value = oldValue.Value.Value then
                        Error([ DuplicateEnumValue(v) ])
                    else
                        Ok ()
                ) ()

                // Assign a new value if not provided
                |>> (fun () ->
                    match v.Value, newMaxValue with
                    | Some vv, _ ->
                        newMaxValue <- Some (max (defaultArg newMaxValue Int32.MinValue) vv)
                    | None, Some c ->
                        v.Value <- Some(c + 1)
                        newMaxValue <- v.Value
                    | None, None ->
                        v.Value <- Some(0)
                        newMaxValue <- v.Value
                )

                // Add the accepted value to a list so that the next value (if any)
                // will be checked against this one too.
                |>> (fun () -> allValues <- v :: allValues)

            ) ()

            |>> (fun _ -> (newMaxValue, allValues))

        )

    member val Name = name
    
    member val BaseType: YANGTypeRef option = None with get, set
    
    member this.PrimitiveType
        with get() =
            match this.BaseType with
            | None -> this
            | Some b -> b.ResolvedType.PrimitiveType

    member this.Default
        with get() =
            match _default, this.BaseType with
            | None, Some b -> b.ResolvedType.Default
            | None, None -> null
            | Some v, _ -> v
        and set(v) =
            _default <- Some v

    member this.Description
        with get() =
            match _description, this.BaseType with
            | None, Some b -> b.ResolvedType.Description
            | None, None -> null
            | Some v, _ -> v
        and set(v) =
            _description <- Some v
    
    member this.Reference
        with get() =
            match _reference, this.BaseType with
            | None, Some b -> b.ResolvedType.Reference
            | None, None -> null
            | Some v, _ -> v
        and set(v) =
            _reference <- Some v
    
    member this.Status
        with get() =
            match _status, this.BaseType with
            | None, Some b -> b.ResolvedType.Status
            | None, None -> YANGStatus.Current
            | Some v, _ -> v
        and set(v) =
            _status <- Some v
    
    member this.Units
        with get() =
            match _units, this.BaseType with
            | None, Some b -> b.ResolvedType.Units
            | None, None -> null
            | Some v, _ -> v
        and set(v) =
            _units <- Some v

    member val Restrictions = ResizeArray<YANGTypeRestriction>()

    member this.SetProperty<'T>(prop: YANGTypeProperty<'T>, value: 'T) =
        _properties.Add(prop.Name, value)

    member internal this.SetProperty(prop: string, value: obj) =
        _properties.Add(prop, value)

    member this.GetProperty<'T>(prop: YANGTypeProperty<'T>) : 'T option =
        this.GetProperty(prop, false)

    member internal this.GetProperty<'T>(prop: YANGTypeProperty<'T>, local: bool) : 'T option =
        match _properties.TryGetValue(prop.Name), this.BaseType, local with
        | (true, value), _, _ -> Some(value :?> 'T)
        | _, Some(b), false -> b.GetProperty(prop)
        | _ -> None

    abstract member CanBeRestrictedWith: YANGTypeRestriction -> bool
    default this.CanBeRestrictedWith(r: YANGTypeRestriction) =
        match this.BaseType with
        | Some(b) -> b.ResolvedType.CanBeRestrictedWith(r)
        | None -> invalidOp (sprintf "Missing restriction filter logic for type %A" this.Name)

    member this.IsValid o =
        let violatedRestriction =
            this.Restrictions
            |> Seq.tryFind (fun r -> not (r.IsValid o))
        match violatedRestriction, this.BaseType with
        | Some r, _ -> Error r
        | None, Some b -> b.ResolvedType.IsValid o
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
        | Some(b) -> b.ResolvedType.ParseCore(actualType, str)
        | None -> invalidOp (sprintf "Missing parsing logic for type %A" this.Name)

    abstract member SerializeCore: YANGType * obj -> string option
    default this.SerializeCore(actualType: YANGType, o: obj) =
        match this.BaseType with
        | Some(b) -> b.ResolvedType.SerializeCore(actualType, o)
        | None -> invalidOp (sprintf "Missing serializing logic for type %A" this.Name)

    abstract member CheckRequiredPropertiesCore: YANGType -> Result<unit, SchemaError list>
    default this.CheckRequiredPropertiesCore(actualType) =
        match this.BaseType with
        | Some(b) -> b.ResolvedType.CheckRequiredPropertiesCore(actualType)
        | None -> Ok()

    override this.EnsureRefs() =

        // Do nothing if this is a primitive type
        if this.PrimitiveType = this then
            Ok ()
        else

            // First, check the base type
            (
                match this.BaseType with
                | Some b -> b.EnsureRefs()
                | None -> Ok()
            )

            // Then check that all the required properties have been provided
            >>= this.CheckRequiredProperties

            // Check that all the restrictions can be applied to this type
            >>= (fun () ->
                foldResult (fun _ r ->
                    if this.CanBeRestrictedWith(r) then
                        Ok()
                    else
                        Error([ InvalidTypeRestriction(r.OriginalStatement.Value, this) ])
                ) () this.Restrictions
            )

            // If this type is an enum, check the validity of the names
            // and provide autocomputed values
            >>= (fun () ->
                if this.PrimitiveType = YANGPrimitiveTypes.Enumeration then
                    checkEnumValuesForType this
                    |>> ignore
                else
                    Ok ()
            )

            // If a default value has been provided, check that it's valid
            >>= (fun () ->
                if not (isNull this.Default) then
                    match this.IsValid(this.Default) with
                    | Ok () -> Ok ()
                    | Error(restriction) -> Error([ InvalidDefault(this, restriction) ])
                else
                    Ok()
            )


/// Reference to a YANG type.
/// A reference can include additional properties and restrictions.
and YANGTypeRef internal (resolvedType: YANGType option, prefix: string option, name: string, scope: Scope) =
    inherit YANGNode()

    let mutable _resolvedType = resolvedType
    let _additionalProperties = Dictionary<string, obj>()

    static member FromExistingType(existingType) =
        YANGTypeRef(Some existingType, None, Unchecked.defaultof<_>, Unchecked.defaultof<_>)

    member val AdditionalRestrictions = ResizeArray<YANGTypeRestriction>()

    member this.ResolvedType
        with get() =
            match this.Resolve() with
            | Some t -> t
            | None -> invalidOp "Invalid type reference."

    member this.SetProperty<'T>(prop: YANGTypeProperty<'T>, value: 'T) =
        _additionalProperties.Add(prop.Name, value)

    member this.GetProperty<'T>(prop: YANGTypeProperty<'T>) : 'T option =
        match _additionalProperties.TryGetValue(prop.Name) with
        | (true, x) -> Some(x :?> 'T)
        | _ -> None

    member internal this.Resolve() : YANGType option =
        match _resolvedType with
        | Some _ -> _resolvedType
        | None ->
            match scope.ResolveTypeRef(prefix, name) with
            | None -> None
            | Some t ->
                
                // We create a new type containing the additional properties and restrictions.
                // If none of them have been provided, use the resolved type as-is, and avoid
                // an additional allocation.
                if _additionalProperties.Count = 0 && this.AdditionalRestrictions.Count = 0 then
                    _resolvedType <- Some t
                else
                    let newType =
                        YANGType(
                            t.Name,
                            BaseType = Some (YANGTypeRef.FromExistingType(t)),
                            OriginalStatement = this.OriginalStatement
                        )
                    _additionalProperties |> Seq.iter (fun pair -> newType.SetProperty(pair.Key, pair.Value))
                    this.AdditionalRestrictions |> Seq.iter (fun r -> newType.Restrictions.Add(r))
                    _resolvedType <- Some newType
                
                _resolvedType

    override this.EnsureRefs() =
        // Checks that the type can be resolved and is valid
        match this.Resolve() with
        | None -> Error([ UnresolvedTypeRef(this.OriginalStatement.Value, name) ])
        | Some t -> t.EnsureRefs()


/// Static class with all the primitive types defined by YANG.
and YANGPrimitiveTypes private () =
    
    static let makeIntegralType name (tryParse: string -> bool * 'a) =
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
                    if isNull o || not (o :? 'a)then
                        None
                    else
                        Some(o.ToString())

                override this.CanBeRestrictedWith r =
                    r :? YANGRangeRestriction

        }

    static let allValues (t: YANGType) (prop: YANGTypeProperty<IList<_>>) =
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

    static member internal AllValues t prop =
        allValues t prop

    static member val Empty = {
        new YANGType({ Namespace = YANGNamespace.Default; Name = "empty" }) with

            override this.ParseCore(_, _) =
                None

            override this.SerializeCore(_, _) =
                None

            override this.CanBeRestrictedWith r =
                false

    }

    static member val Boolean = {
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

    static member val Int8 = makeIntegralType "int8" System.SByte.TryParse
    static member val Int16 = makeIntegralType "int16" System.Int16.TryParse
    static member val Int32 = makeIntegralType "int32" System.Int32.TryParse
    static member val Int64 = makeIntegralType "int64" System.Int64.TryParse
    static member val UInt8 = makeIntegralType "uint8" System.Byte.TryParse
    static member val UInt16 = makeIntegralType "uint16" System.UInt16.TryParse
    static member val UInt32 = makeIntegralType "uint32" System.UInt32.TryParse
    static member val UInt64 = makeIntegralType "uint64" System.UInt64.TryParse

    static member val Decimal64 = {
        new YANGType({ Namespace = YANGNamespace.Default; Name = "decimal64" }) with

            override this.ParseCore(actualType, str) =
                let digits =
                    match actualType.GetProperty(YANGTypeProperties.FractionDigits) with
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
                    match actualType.GetProperty(YANGTypeProperties.FractionDigits) with
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
                match actualType.GetProperty(YANGTypeProperties.FractionDigits) with
                    | Some(d) when d >= 1 && d <= 18 -> Ok()
                    | Some _ -> Error([ ArgumentParserError(actualType.OriginalStatement.Value, "Invalid value for required property \"fraction-digits\": allowed values are from 0 to 18.") ])
                    | None -> Error([ MissingRequiredStatement(actualType.OriginalStatement.Value, "fraction-digits") ])

    }

    static member val String = {
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

    static member val Binary = {
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

    static member val Enumeration = {
        new YANGType({ Namespace = YANGNamespace.Default; Name = "enumeration" }) with

            override this.ParseCore(actualType, str) =
                if isNull str then
                    None
                else
                    allValues actualType YANGTypeProperties.EnumValues
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
                    allValues actualType YANGTypeProperties.EnumValues
                    |> Seq.tryPick chooser
                )

            override this.CanBeRestrictedWith _ =
                false

            override this.CheckRequiredPropertiesCore(actualType) =
                match actualType.GetProperty(YANGTypeProperties.EnumValues) with
                | Some l when l.Count > 0 -> Ok()
                | _ -> Error([ MissingRequiredStatement(actualType.OriginalStatement.Value, "enum") ])

    }

    static member val Union = {
        new YANGType({ Namespace = YANGNamespace.Default; Name = "union" }) with

            override this.ParseCore(actualType, str) =
                if isNull str then
                    None
                else
                    allValues actualType YANGTypeProperties.UnionMembers
                    |> Seq.tryPick (fun t -> t.ResolvedType.Parse(str))

            override this.SerializeCore(actualType, str) =
                if isNull str then
                    None
                else
                    allValues actualType YANGTypeProperties.UnionMembers
                    |> Seq.tryPick (fun t -> t.ResolvedType.Serialize(str))

            override this.CanBeRestrictedWith _ =
                false

            override this.CheckRequiredPropertiesCore(actualType) =
                match actualType.GetProperty(YANGTypeProperties.UnionMembers) with
                | Some l when l.Count > 0 ->
                    
                    // Unions cannot have members of type empty
                    l |> Seq.tryPick (fun t ->
                        if Object.ReferenceEquals(t.ResolvedType.PrimitiveType, YANGPrimitiveTypes.Empty) then
                            Some t
                        else
                            None
                    )
                    |> function
                       | Some t -> Error([ UnexpectedStatement(t.OriginalStatement.Value) ])
                       | None -> Ok()

                | _ -> Error([ MissingRequiredStatement(actualType.OriginalStatement.Value, "type") ])

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
        | "union" -> Some(YANGPrimitiveTypes.Union)
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
    member val Revision: YANGModuleRevision option = None with get, set

    member val DataNodes = ResizeArray<YANGDataNode>()

    member val ExportedTypes = Dictionary<YANGName, YANGType>()

    override this.EnsureRefs() =
        this.DataNodes
        |> Seq.cast<YANGNode>
        |> Seq.append (this.ExportedTypes.Values |> Seq.cast<YANGNode>)
        |> foldResult (fun _ node -> node.EnsureRefs()) ()


and YANGModuleRevision(date: DateTime) =
    inherit YANGNode()

    member val Date = date
    member val Description: string = null with get, set
    member val Reference: string = null with get, set

    override this.EnsureRefs() =
        Ok()


and YANGContainer(name: YANGName) =
    inherit YANGDataNode()

    member this.Name = name
    member val Presence: string = null with get, set
    member val Description: string = null with get, set
    member val Reference: string = null with get, set
    member val Status = YANGStatus.Current with get, set
    member val DataNodes = ResizeArray<YANGDataNode>()

    override this.EnsureRefs() =
        this.DataNodes
        |> foldResult (fun _ node -> node.EnsureRefs()) ()
    

and YANGLeaf(name: YANGName) =
    inherit YANGDataNode()

    member this.Name = name
    member val Type = Unchecked.defaultof<YANGTypeRef> with get, set
    member val Units: string = null with get, set
    member val Default: obj = null with get, set
    member val Mandatory: bool = false with get, set
    member val Description: string = null with get, set
    member val Reference: string = null with get, set
    member val Status = YANGStatus.Current with get, set

    override this.EnsureRefs() =

        // Check that the type is valid
        this.Type.EnsureRefs()

        // If a default value has been provided, check that it's valid
        >>= (fun () ->
            if not (isNull this.Default) then
                match this.Type.ResolvedType.IsValid(this.Default) with
                | Ok _ -> Ok()
                | Error(restriction) -> Error([ InvalidLeafDefault(this, restriction) ])
            else
                Ok()
        )


and YANGLeafList(name: YANGName) =
    inherit YANGDataNode()

    member this.Name = name
    member val Description: string = null with get, set
    member val MaxElements = Int32.MaxValue with get, set
    member val MinElements = 0 with get, set
    member val OrderedBy = YANGListOrderedBy.System with get, set
    member val Reference: string = null with get, set
    member val Status = YANGStatus.Current with get, set
    member val Type = Unchecked.defaultof<YANGTypeRef> with get, set
    member val Units: string = null with get, set

    override this.EnsureRefs() =
        this.Type.EnsureRefs()


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

    override this.EnsureRefs() =
        this.DataNodes
        |> foldResult (fun _ node -> node.EnsureRefs()) ()