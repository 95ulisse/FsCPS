module internal FsCPS.Yang.SchemaParsers

open System
open System.Collections.Generic
open System.IO
open System.Text
open System.Text.RegularExpressions
open FsCPS
open FsCPS.Yang.Combinators
open FsCPS.Yang.Model

#nowarn "58" // Indentation

// ------------------------------------------------------------------
// Argument parsers
// ------------------------------------------------------------------

/// Argument parser that accepts any string.
let any : SchemaParser<string> =
    fun ctx ->
        Ok(ctx.Statement.Argument.Value)

/// Helper function that transforms an FParsec parser in an argument parser suitable
/// for a `StatementSpec` construction.
let arg (parser: FParsec.Primitives.Parser<_, unit>) : SchemaParser<_> =
    fun ctx ->
        match FParsec.CharParsers.run parser ctx.Statement.Argument.Value with
        | FParsec.CharParsers.Success(parsedArg, _, _) -> Ok(parsedArg)
        | FParsec.CharParsers.Failure(e, _, _) -> Error([ ArgumentParserError(ctx.Statement, e) ])

/// Argument parser for an unqualified identifier.
let unqualifiedIdentifier = arg StatementParsers.id

/// Argument parser for an unsigned int.
let uint = arg FParsec.CharParsers.puint32

/// Argument parser for a positive integer.
let positiveInt =
    fun ctx ->
        arg FParsec.CharParsers.pint32 ctx
        |> Result.bind (fun res ->
            if res < 0 then
                Error([ ArgumentParserError(ctx.Statement, "Expected non negative integer.") ])
            else
                Ok(res)
        )

/// Argument parser for a positive integer or the string "unbounded",
/// which gets parsed to Int32.MaxValue.
let positiveIntOrUnbounded =
    let rawParser =
        FParsec.Primitives.choice [
            FParsec.CharParsers.pint32;
            FParsec.Primitives.(|>>) (FParsec.CharParsers.pstring "unbounded") (fun _ -> Int32.MaxValue)
        ]
    fun ctx ->
        arg rawParser ctx
        |> Result.bind (fun res ->
            if res < 0 then
                Error([ ArgumentParserError(ctx.Statement, "Expected non negative integer.") ])
            else
                Ok(res)
        )

/// Argument parser for a boolean.
let boolean : SchemaParser<bool> =
    fun ctx ->
        match Boolean.TryParse(ctx.Statement.Argument.Value) with
        | (true, b) -> Ok(b)
        | _ -> Error([ ArgumentParserError(ctx.Statement, "Invalid boolean value.") ])

/// Argument parser for a qualified identifier.
/// Parses an identifier and as namespace uses the current namespace of the parsing context.
let identifier : SchemaParser<YANGName> =
    fun ctx ->
        match unqualifiedIdentifier ctx with
        | Ok(unqualifiedName) ->
            let fullName = {
                Namespace = ctx.Scope.Module.Namespace;
                Name = unqualifiedName
            }
            Ok(fullName)
        | Error(l) -> Error(l)

/// Argument parser for a reference to a type.
let typeRef : SchemaParser<YANGType> =
    fun ctx ->
        match arg StatementParsers.ref ctx with
        | Error(l) -> Error(l)
        | Ok((prefix, name)) -> 
            match ctx.ResolveTypeRef(prefix, name) with
            | Some(t) -> Ok(t)
            | None -> Error([ UnresolvedTypeRef(ctx.Statement, ctx.Statement.Argument.Value) ])

/// Argument parser for a Uri.
let uri : SchemaParser<Uri> =
    fun ctx ->
        match Uri.TryCreate(ctx.Statement.Argument.Value, UriKind.Absolute) with
        | (true, uri) -> Ok(uri)
        | _ -> Error([ ArgumentParserError(ctx.Statement, "Invalid Uri.") ])

/// Argument parser for a Regex.
let regex : SchemaParser<Regex> =
    fun ctx ->
        try
            Ok(Regex(ctx.Statement.Argument.Value, RegexOptions.Compiled))
        with
        | _ -> Error([ ArgumentParserError(ctx.Statement, "Invalid Regex.") ])

/// Argument parser for a status statement.
let status : SchemaParser<YANGStatus> =
    fun ctx ->
        match ctx.Statement.Argument.Value with
        | "current" -> Ok(YANGStatus.Current)
        | "deprecated" -> Ok(YANGStatus.Deprecated)
        | "obsolete" -> Ok(YANGStatus.Obsolete)
        | _ -> Error([ ArgumentParserError(ctx.Statement, "Invalid status. The passible values are `current`, `deprecated` or `obsolete`.") ])

/// Argument parser for a ordered-by statement.
let orderedBy : SchemaParser<YANGListOrderedBy> =
    fun ctx ->
        match ctx.Statement.Argument.Value with
        | "system" -> Ok(YANGListOrderedBy.System)
        | "user" -> Ok(YANGListOrderedBy.User)
        | _ -> Error([ ArgumentParserError(ctx.Statement, "Invalid ordering. The passible values are `system` or `user`.") ])

/// Helper operator to execute an action after a parser only if it succeeds.
let (>=>) (spec: StatementSpec<_>) f : StatementSpec<_> =
    {
        Name = spec.Name;
        Cardinality = spec.Cardinality;
        Parser = (fun ctx ->
            match spec.Parser ctx with
            | Ok(res) -> f res ctx
            | Error(l) -> Error(l)
        )
    }



// ------------------------------------------------------------------
// Helper types
// ------------------------------------------------------------------

type YANGTypeRef(t: YANGType) =
    inherit YANGNode()

    let _additionalProperties = Dictionary<string, obj>()

    member val Value = t with get, set
    member val AdditionalRestrictions = ResizeArray<YANGTypeRestriction>()

    member this.ToNewType(name) =
        this.MergeInto(YANGType(name))

    member this.MergeInto(t: YANGType) =
        t.BaseType <- Some this.Value
        t.Restrictions.AddRange(this.AdditionalRestrictions)
        _additionalProperties |> Seq.iter (fun pair -> t.SetProperty(pair.Key, pair.Value))
                
        // Check that the required properties have been provided
        t.CheckRequiredProperties()

    member this.SetProperty<'T>(prop: YANGTypeProperty<'T>, value: 'T) =
        _additionalProperties.Add(prop.Name, value)

    member this.GetProperty<'T>(prop: YANGTypeProperty<'T>) =
        match _additionalProperties.TryGetValue(prop.Name) with
        | (true, x) -> Some(x :?> 'T)
        | _ -> None

type YANGImport(moduleName: string) =
    inherit YANGNode()
    member val ModuleName = moduleName
    member val Prefix: string = null with get, set
    member val RevisionDate: DateTime option = None with get, set



// ------------------------------------------------------------------
// Actual parsers
// ------------------------------------------------------------------

/// Shorthand function to create a spec for a statement representing a type restriction.
/// Automatically configures the optional substatements `description`, `error-app-tag`,
/// `error-message` and `reference`.
let createRestrictionSpec name (ctor: 'a -> #YANGTypeRestriction) (argParser: SchemaParser<'a>) =
    createSpec name ctor argParser (anyOf [
        prop any Optional <@ fun x -> x.Description @>;
        prop any Optional <@ fun x -> x.ErrorAppTag @>;
        prop any Optional <@ fun x -> x.ErrorMessage @>;
        prop any Optional <@ fun x -> x.Reference @>;
    ])

    // Before returning the restriction, check that the enclosing type supports it
    >=> (fun r ctx ->
        let t = ctx.Scope.ParentNode :?> YANGTypeRef
        if t.Value.CanBeRestrictedWith(r) then
            Ok(r)
        else
            Error([ InvalidTypeRestriction(r.OriginalStatement.Value, t.Value) ])
    )

/// Checks if an enum value is valid: checks the uniqueness of the name
/// and of the value. If the value has not been provided, it automatically computes
/// and assignes the next one.
let checkNewEnumValue (node: YANGTypeRef) (enumValue: YANGEnumValue) =
    let currentValues =
        match node.GetProperty(YANGTypeProperties.EnumValues) with
        | Some l -> l :> seq<YANGEnumValue>
        | _ -> Seq.empty
    
    let mutable computedValue = None
    let mutable result = Ok()
    Seq.append currentValues (YANGPrimitiveTypes.AllEnumValues node.Value)
    |> Seq.forall (fun v ->
        if v.Name = enumValue.Name then
            result <- Error([ DuplicateEnumName(enumValue) ])
            false
        else if enumValue.Value.IsSome && enumValue.Value.Value = v.Value.Value then
            result <- Error([ DuplicateEnumValue(enumValue) ])
            false
        else
            computedValue <-
                match computedValue with
                | Some(c) -> Some(max c v.Value.Value)
                | None -> Some(v.Value.Value)
            true
    ) |> ignore

    match enumValue.Value, computedValue with
    | Some _, _ -> ()
    | None, Some c -> enumValue.Value <- Some(c + 1)
    | None, None -> enumValue.Value <- Some(0)

    result



let prangeRestriction : StatementSpec<YANGRangeRestriction> =
    createRestrictionSpec
        "range"
        YANGRangeRestriction
        (arg StatementParsers.rangeDescription)

let plengthRestriction : StatementSpec<YANGLengthRestriction> =
    createRestrictionSpec
        "length"
        YANGLengthRestriction
        (arg StatementParsers.lengthRangeDescription)

let ppatternRestriction : StatementSpec<YANGPatternRestriction> =
    createRestrictionSpec
        "pattern"
        YANGPatternRestriction
        regex

let penum : StatementSpec<YANGEnumValue> =
    createSpec
        "enum"
        YANGEnumValue
        unqualifiedIdentifier
        (anyOf [
            prop any                                          Optional <@ fun x -> x.Description @>;
            prop any                                          Optional <@ fun x -> x.Reference @>;
            prop status                                       Optional <@ fun x -> x.Status @>;
            property "value" (arg FParsec.CharParsers.pint32) Optional (fun node value _ ->
                node.Value <- Some value
                Ok()
            );
        ])

let ptype : StatementSpec<YANGTypeRef> =
    createSpec
        "type"
        YANGTypeRef
        typeRef
        (anyOf [
            property "fraction-digits" uint Optional (fun node digits _ ->
                node.SetProperty(YANGTypeProperties.FractionDigits, int digits)
                Ok()
            );

            child penum Many (fun node enumValue _ ->
                match checkNewEnumValue node enumValue with
                | Ok() ->
                    match node.GetProperty(YANGTypeProperties.EnumValues) with
                    | Some l -> l.Add(enumValue)
                    | _ -> node.SetProperty(YANGTypeProperties.EnumValues, ResizeArray([ enumValue ]) :> IList<_>)
                    Ok()
                | Error(l) -> Error(l)
            );

            chld prangeRestriction   Optional <@ fun x -> x.AdditionalRestrictions @>;
            chld plengthRestriction  Optional <@ fun x -> x.AdditionalRestrictions @>;
            chld ppatternRestriction Many     <@ fun x -> x.AdditionalRestrictions @>;
        ])

let ptypedef : StatementSpec<YANGType> =
    createSpec
        "typedef"
        YANGType
        identifier
        (anyOf [
            child ptype Required (fun node value _ -> value.MergeInto(node));

            property "default" any    Optional (fun x value _ -> x.Default <- value; Ok());
            prop               any    Optional <@ fun x -> x.Description @>;
            prop               any    Optional <@ fun x -> x.Reference @>;
            prop               status Optional <@ fun x -> x.Status @>;
            prop               any    Optional <@ fun x -> x.Units @>;
        ])

    // If a default value has been provided, check that it's valid
    >=> (fun t ctx ->
        if not (isNull t.Default) then
            match t.IsValid(t.Default) with
            | Ok _ -> Ok(t)
            | Error(restriction) -> Error([ InvalidDefault(t, restriction) ])
        else
            Ok(t)
    )

    // Registers the new type in the context automatically
    >=> (fun t ctx ->
        ctx.RegisterType(t)
        |> Result.map (fun _ -> t)
    )


// Parsers for all the data nodes.
// They are all forwarded because they are all mutually recursive.
let pcontainer, pcontainerRef: StatementSpec<YANGContainer> * _  = createSpecForwaredToRef "container"  Required
let pleaf,      pleafRef:      StatementSpec<YANGLeaf> * _       = createSpecForwaredToRef "leaf"       Required
let pleaflist,  pleaflistRef:  StatementSpec<YANGLeafList> * _   = createSpecForwaredToRef "leaf-list"  Required
let plist,      plistRef:      StatementSpec<YANGList> * _       = createSpecForwaredToRef "list"       Required

pcontainerRef :=
    createSpec
        "container"
        YANGContainer
        identifier
        (anyOf [
            // Container-specific properties
            prop any    Optional <@ fun x -> x.Presence @>;

            // Common properties for data nodes
            prop any    Optional <@ fun x -> x.Description @>;
            prop any    Optional <@ fun x -> x.Reference @>;
            prop status Optional <@ fun x -> x.Status @>;

            // Typedefs
            child ptypedef Many (fun _ _ _ -> Ok());

            // Data statements
            chld pcontainer Many <@ fun x -> x.DataNodes @>;
            chld pleaf      Many <@ fun x -> x.DataNodes @>;
            chld pleaflist  Many <@ fun x -> x.DataNodes @>;
            chld plist      Many <@ fun x -> x.DataNodes @>;
        ])

pleafRef :=
    createSpec
        "leaf"
        YANGLeaf
        identifier
        (anyOf [
            // Leaf-specific properties
            property "default" any     Optional (fun x value _ -> x.Default <- value; Ok());
            prop               any     Optional <@ fun x -> x.Units @>;
            prop               boolean Optional <@ fun x -> x.Mandatory @>;
            child ptype Required (fun node value _ -> value.ToNewType(value.Value.Name));

            // Common properties for data nodes
            prop any     Optional <@ fun x -> x.Description @>;
            prop any     Optional <@ fun x -> x.Reference @>;
            prop status  Optional <@ fun x -> x.Status @>;
        ])

    // If a default value has been provided, check that it's valid
    >=> (fun leaf ctx ->
        if not (isNull leaf.Default) then
            match leaf.Type.IsValid(leaf.Default) with
            | Ok _ -> Ok(leaf)
            | Error(restriction) -> Error([ InvalidLeafDefault(leaf, restriction) ])
        else
            Ok(leaf)
    )

    // Mandatory leafs cannot have a default value
    >=> (fun leaf ctx ->
        if leaf.Mandatory && not (isNull leaf.Default) then
            Error([ SchemaError(leaf, "Mandatory leafs cannot have a default value.") ])
        else
            Ok(leaf)
    )

pleaflistRef :=
    createSpec
        "leaf-list"
        YANGLeafList
        identifier
        (anyOf [
            // LeafList-specific properties
            prop positiveInt            Optional <@ fun x -> x.MinElements @>
            prop positiveIntOrUnbounded Optional <@ fun x -> x.MaxElements @>
            prop orderedBy              Optional <@ fun x -> x.OrderedBy @>;
            prop any                    Optional <@ fun x -> x.Units @>;
            child ptype Required (fun node value _ -> value.ToNewType(value.Value.Name));

            // Common properties for data nodes
            prop any     Optional <@ fun x -> x.Description @>;
            prop any     Optional <@ fun x -> x.Reference @>;
            prop status  Optional <@ fun x -> x.Status @>;
        ])

plistRef :=
    createSpec
        "list"
        YANGList
        identifier
        (anyOf [
            // List-specific properties
            prop positiveInt            Optional <@ fun x -> x.MinElements @>
            prop positiveIntOrUnbounded Optional <@ fun x -> x.MaxElements @>
            prop orderedBy              Optional <@ fun x -> x.OrderedBy @>;

            // Common properties for data nodes
            prop any     Optional <@ fun x -> x.Description @>;
            prop any     Optional <@ fun x -> x.Reference @>;
            prop status  Optional <@ fun x -> x.Status @>;

            // Typedefs
            child ptypedef Many (fun _ _ _ -> Ok());

            // Data statements
            chld pcontainer Many <@ fun x -> x.DataNodes @>;
            chld pleaf      Many <@ fun x -> x.DataNodes @>;
            chld pleaflist  Many <@ fun x -> x.DataNodes @>;
            chld plist      Many <@ fun x -> x.DataNodes @>;
        ])

let pmodulerevision : StatementSpec<YANGModuleRevision> =
    createSpec
        "revision"
        YANGModuleRevision
        (arg StatementParsers.dateArg)
        (anyOf [
            prop any Optional <@ fun x -> x.Description @>;
            prop any Optional <@ fun x -> x.Reference @>;
        ])

let pimport : StatementSpec<_> =
    createSpec
        "import"
        YANGImport
        unqualifiedIdentifier
        (anyOf [
            prop unqualifiedIdentifier Required <@ fun x -> x.Prefix @>;
            property "revision-date" (arg StatementParsers.dateArg) Optional (fun node d _ ->
                node.RevisionDate <- Some d
                Ok()
            )
        ])

    // Resolve the import
    >=> (fun import ctx ->
        ctx.ResolveImport(import.ModuleName, import.RevisionDate)
        |> Result.map (fun m -> (import, m))
    )

    // Import the contents of the module in the current context
    >=> (fun (import, m) ctx ->
        ctx.RegisterImportedModule(m, import.Prefix)
    )

let pmodule : StatementSpec<YANGModule> =
    createSpec
        "module"
        YANGModule
        unqualifiedIdentifier
        (unorderedGroups [

            // Module header statements
            [
                property "yang-version" any Optional (fun node value ctx ->
                    if value <> "1" then
                        Error([ UnsupportedYangVersion(ctx.Statement, value) ])
                    else
                        Ok()
                );
                property "namespace" uri Required (fun node value ctx ->
                    node.Namespace <- { Module = node; Uri = value }
                    Ok()
                );
                property "prefix" unqualifiedIdentifier Required (fun node value ctx ->
                    node.Prefix <- value
                    ctx.Scope.Prefixes.Add(node.Prefix, node)
                    Ok()
                )
            ];

            // Linkage statements
            [
                child pimport Many (fun _ _ _ -> Ok());
            ];

            // Meta statements
            [
                prop any Optional <@ fun x -> x.Contact @>;
                prop any Optional <@ fun x -> x.Organization @>;
                prop any Optional <@ fun x -> x.Description @>;
                prop any Optional <@ fun x -> x.Reference @>;
            ];

            // Revision statements
            [
                child pmodulerevision Many (fun node rev _ ->
                    // We are interested only on the most recent revision
                    match node.Revision with
                    | None -> node.Revision <- Some rev
                    | Some oldRev ->
                        if oldRev.Date <= rev.Date then
                            node.Revision <- Some rev
                    Ok()
                );
            ];

            // Body statements
            [
                // Typedefs
                child ptypedef Many (fun x t _ -> x.ExportedTypes.Add(t.Name, t); Ok());

                // Data statements
                chld pcontainer Many <@ fun x -> x.DataNodes @>;
                chld pleaf      Many <@ fun x -> x.DataNodes @>;
                chld pleaflist  Many <@ fun x -> x.DataNodes @>;
                chld plist      Many <@ fun x -> x.DataNodes @>;
            ]
        ])

/// Final root parser.
let rootParser = pmodule.Parser