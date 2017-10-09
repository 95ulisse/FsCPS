module internal FsCPS.Yang.SchemaParsers

open System
open System.Collections.Generic
open System.IO
open System.Text
open System.Text.RegularExpressions
open FsCPS
open FsCPS.Yang.Combinators

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

/// Argument parser for a reference to an entity.
let entityRef : SchemaParser<_> =
    fun ctx ->
        match arg StatementParsers.ref ctx with
        | Ok((prefix, name)) ->  Ok(None, prefix, name, ctx.Scope)
        | Error(l) -> Error(l)

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

/// Argument parser for a node path
let nodePath : SchemaParser<_> =
    fun ctx ->
        match arg StatementParsers.nodePath ctx with
        | Ok(path) ->  Ok(path, ctx.Scope)
        | Error(l) -> Error(l)

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

type YANGImport(moduleName: string) =
    inherit YANGNode()
    member val ModuleName = moduleName
    member val Prefix: string = null with get, set
    member val RevisionDate: DateTime option = None with get, set

    override this.EnsureRefs() =
        Ok()



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

let ptype, ptypeRef : StatementSpec<YANGTypeRef> * _ = createSpecForwaredToRef "type" Required
ptypeRef :=
    createSpec
        "type"
        YANGTypeRef
        entityRef
        (anyOf [
            property "fraction-digits" uint Optional (fun node digits _ ->
                node.SetProperty(YANGTypeProperties.FractionDigits, int digits)
                Ok()
            );

            child penum Many (fun node enumValue _ ->
                match node.GetProperty(YANGTypeProperties.EnumValues) with
                | Some l -> l.Add(enumValue)
                | _ -> node.SetProperty(YANGTypeProperties.EnumValues, ResizeArray([ enumValue ]) :> IList<_>)
                Ok()
            );

            child ptype Many (fun node unionMember _ ->
                match node.GetProperty(YANGTypeProperties.UnionMembers) with
                | Some l  -> l.Add(unionMember)
                | _ -> node.SetProperty(YANGTypeProperties.UnionMembers, ResizeArray([ unionMember ]) :> IList<_>)
                Ok()
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
            child ptype Required (fun node value _ ->
                node.BaseType <- Some value
                Ok()
            );

            property "default" any    Optional (fun x value _ -> x.Default <- value; Ok());
            prop               any    Optional <@ fun x -> x.Description @>;
            prop               any    Optional <@ fun x -> x.Reference @>;
            prop               status Optional <@ fun x -> x.Status @>;
            prop               any    Optional <@ fun x -> x.Units @>;
        ])

    // Registers the new type in the context automatically
    >=> (fun t ctx ->
        ctx.Scope.RegisterType(t)
        |> Result.map (fun _ -> t)
    )


// Parsers for all the data nodes.
// They are all forwarded because they are all mutually recursive.
let pleaf,      pleafRef:      StatementSpec<YANGLeaf> * _         = createSpecForwaredToRef "leaf"       Required
let pleaflist,  pleaflistRef:  StatementSpec<YANGLeafList> * _     = createSpecForwaredToRef "leaf-list"  Required
let pcontainer, pcontainerRef: StatementSpec<YANGContainer> * _    = createSpecForwaredToRef "container"  Required
let plist,      plistRef:      StatementSpec<YANGList> * _         = createSpecForwaredToRef "list"       Required
let pgrouping,  pgroupingRef:  StatementSpec<YANGGrouping> * _     = createSpecForwaredToRef "grouping"   Required
let puses,      pusesRef:      StatementSpec<YANGGroupingRef> * _  = createSpecForwaredToRef "uses"       Required
let paugment,   paugmentRef:   StatementSpec<YANGAugmentation> * _ = createSpecForwaredToRef "augment"    Required

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
            child ptype Required (fun node value _ -> node.Type <- value; Ok());

            // Common properties for data nodes
            prop any     Optional <@ fun x -> x.Description @>;
            prop any     Optional <@ fun x -> x.Reference @>;
            prop status  Optional <@ fun x -> x.Status @>;
        ])

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
            child ptype Required (fun node value _ -> node.Type <- value; Ok());

            // Common properties for data nodes
            prop any     Optional <@ fun x -> x.Description @>;
            prop any     Optional <@ fun x -> x.Reference @>;
            prop status  Optional <@ fun x -> x.Status @>;
        ])

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

            // Groupings
            child pgrouping Many (fun _ _ _ -> Ok());
            chld  puses     Many <@ fun x -> x.DataNodes @>;
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

            // Groupings
            child pgrouping Many (fun _ _ _ -> Ok());
            chld  puses     Many <@ fun x -> x.DataNodes @>;
        ])

pgroupingRef :=
    createSpec
        "grouping"
        YANGGrouping
        identifier
        (anyOf [
            // Description properties
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

            // Groupings
            child pgrouping Many (fun _ _ _ -> Ok());
            chld  puses     Many <@ fun x -> x.DataNodes @>;
        ])

    // Registers the new grouping in the context automatically
    >=> (fun g ctx ->
        ctx.Scope.RegisterGrouping(g)
        |> Result.map (fun _ -> g)
    )

pusesRef :=
    createSpec
        "uses"
        YANGGroupingRef
        entityRef
        (anyOf [
            // Description properties
            prop any     Optional <@ fun x -> x.Description @>;
            prop any     Optional <@ fun x -> x.Reference @>;
            prop status  Optional <@ fun x -> x.Status @>;

            // Augment
            child paugment Optional (fun node a ctx -> node.Augmentation <- Some a; Ok())
        ])

paugmentRef :=
    createSpec
        "augment"
        YANGAugmentation
        nodePath
        (anyOf [

            // Description properties
            prop any     Optional <@ fun x -> x.Description @>;
            prop any     Optional <@ fun x -> x.Reference @>;
            prop status  Optional <@ fun x -> x.Status @>;

            // Data nodes
            chld pcontainer Many <@ fun x -> x.DataNodes @>;
            chld pleaf      Many <@ fun x -> x.DataNodes @>;
            chld pleaflist  Many <@ fun x -> x.DataNodes @>;
            chld plist      Many <@ fun x -> x.DataNodes @>;
            chld puses      Many <@ fun x -> x.DataNodes @>;

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
        ctx.Scope.RegisterImportedModule(m, import.Prefix)
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

                // Groupings
                child pgrouping Many (fun x g _ -> x.ExportedGroupings.Add(g.Name, g); Ok());
                chld  puses     Many <@ fun x -> x.DataNodes @>;

                // Augmentations
                chld  paugment  Many <@ fun x -> x.Augmentations @>
            ]
        ])

/// Final root parser.
let rootParser = pmodule.Parser