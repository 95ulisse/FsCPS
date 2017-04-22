namespace FsCPS.Yang

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Resources
open System.Text
open FsCPS
open FsCPS.Yang.Model

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

    static member ParseModule(stream: Stream, options) =

        // Parses the string as a tree of statements
        match FParsec.CharParsers.runParserOnStream StatementParsers.root () "" stream Encoding.UTF8 with
        | FParsec.CharParsers.Failure(e, _, _) -> Error([ ParserError(e) ])
        | FParsec.CharParsers.Success(statement, _, _) ->

            // Transforms the statements to the actual schema tree
            let ctx = SchemaParserContext(statement, options)
            rootParser ctx


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
and internal Scope = {
    Module: YANGModule;
    ParentStatement: Statement;
    ParentNode: YANGNode;
    Prefixes: Dictionary<string, YANGModule>;
    Types: Dictionary<YANGName, YANGType>;
}


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
                }
            )

    /// Pops the last scope from the stack.
    member __.PopScope() =
        _scopeStack.Pop()

    /// Returns the type with the given name.
    member this.GetType(name: YANGName) =
        _scopeStack
        |> Seq.tryPick (fun scope ->
            match scope.Types.TryGetValue(name) with
            | (true, t) -> Some(t)
            | _ -> None
        )

    /// Returns the module registered with the given prefix in the current scope.
    member this.GetIncludedModule(prefix: string) =
        _scopeStack
        |> Seq.tryPick (fun scope ->
            match scope.Prefixes.TryGetValue(prefix) with
            | (true, m) -> Some m
            | _ -> None
        )

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
                this.Scope.Types.Add(t.Name, t)
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
            this.GetType({ Namespace = this.Scope.Module.Namespace; Name = name })
        | Some(p), _ when p = this.Scope.Module.Prefix ->
            this.GetType({ Namespace = this.Scope.Module.Namespace; Name = name })

        // We have a prefixed reference, so resolve the module associated to the prefix,
        // then search between it's exported types
        | Some(p), _ ->
            match this.GetIncludedModule(p) with
            | Some m ->
                let fullName = { Namespace = m.Namespace; Name = name }
                match m.ExportedTypes.TryGetValue(fullName) with
                | (true, t) -> Some(t)
                | _ -> None
            | None -> None

    /// Resolves an import and compiles the referenced module.
    member this.ResolveImport(name: string, revision: DateTime option) =
        this.Options.ResolveImport name revision

    /// Registers the given module as available under the given prefix.
    member this.RegisterImportedModule(m: YANGModule, prefix: string) =
        match this.GetIncludedModule(prefix) with
        | Some oldModule ->
            Error([ AlreadyUsedPrefix(m.OriginalStatement.Value, oldModule.OriginalStatement.Value) ])
        | None ->
            this.Scope.Prefixes.Add(prefix, m)
            Ok()


/// Result of a parser.
/// It can either be an error or a success, in which case the parsed element is returned.
and internal SchemaParserResult<'T> = Result<'T, SchemaError list>


/// A parser is a function from a parsing context to a result or an error list.
and internal SchemaParser<'T> = SchemaParserContext -> SchemaParserResult<'T>