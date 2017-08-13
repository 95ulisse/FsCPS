namespace ProviderImplementation

#nowarn "10001"

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape
open Microsoft.FSharp.Reflection
open ProviderImplementation
open ProviderImplementation.ProvidedTypes
open ProviderImplementation.ProvidedTypes.UncheckedQuotations
open FsCPS
open FsCPS.Yang
open FsCPS.TypeProviders
open FsCPS.TypeProviders.Runtime
open FsCPS.TypeProviders.Runtime.YANGProviderRuntime

#if DEBUG
open ProviderImplementation.ProvidedTypesTesting
#endif


type internal YANGProviderGenerationContext() =
    let nodeStack = Stack<YANGNode>()
    let typeStack = Stack<ProvidedTypeDefinition>()
    let pathStack = Stack<CPSPath>()

    member __.Push(node, t, path) =
        nodeStack.Push(node)
        typeStack.Push(t)
        pathStack.Push(path)

    member __.Pop() =
        nodeStack.Pop() |> ignore
        typeStack.Pop() |> ignore
        pathStack.Pop() |> ignore

    member __.CurrentNode
        with get() = nodeStack.Peek()

    member __.CurrentType
        with get() = typeStack.Peek()

    member __.CurrentPath
        with get() = pathStack.Peek()

    member __.RootType
        with get() = typeStack |> Seq.last

    member __.TypesSequence
        with get() = typeStack :> seq<_>
        
    member __.IsEmpty
        with get() = nodeStack.Count = 0


[<TypeProvider>]
type YANGProvider(config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()

    let ns = "FsCPS.TypeProviders"
    let asm = Assembly.GetExecutingAssembly()
    
    let yangProviderType = ProvidedTypeDefinition(asm, ns, "YANGProvider", None, HideObjectMethods = true)
    
    let staticParams =
        [
            ProvidedStaticParameter("model", typeof<string>, "");
            ProvidedStaticParameter("fileName", typeof<string>, "");
            ProvidedStaticParameter("rootPath", typeof<string>, "");
        ]

    let nameNormalizerRegex = Regex("(?:^|-|\.)(\w)", RegexOptions.Compiled)


    // Normalizes the name of a YANG node to be suitable as a standard .NET identifier.
    let normalizeName (name: string) =
        nameNormalizerRegex.Replace(name, fun m -> m.Groups.[1].Value.ToUpperInvariant())
            .Trim([| '-'; '.' |])

    let makeOptionType t =
        ProvidedTypeBuilder.MakeGenericType(typedefof<option<_>>, [ t ])

    let makeListType t =
        ProvidedTypeBuilder.MakeGenericType(typedefof<list<_>>, [ t ])

    let makeValidationResultType t =
        ProvidedTypeBuilder.MakeGenericType(typedefof<Result<_, _>>, [ t; typeof<ValidationError> ])

    let patchMethodCallWithOther template replacer expr =

        // We could have used the SpecificCall pattern, but it does not pass the MethodInfo, and we need it.
        let (templateMethod, templateIsGeneric) =
            match template with
            | Lambda(_, Call(_, m, _)) | Call(_, m, _) ->
                if m.IsGenericMethod then
                    (m.GetGenericMethodDefinition(), true)
                else
                    (m, false)
            | _ ->
                failwith "Invalid call template"

        let rec f =
            function
            | Call (receiver, method, args)
                // Checks if it's the method we have to patch
                when (
                      if templateIsGeneric then
                          method.IsGenericMethod && templateMethod = method.GetGenericMethodDefinition()
                      else
                          templateMethod = method) ->

                replacer receiver method (args |> List.map f)

            | ShapeVar v -> Expr.Var(v)
            | ShapeLambda (v, body) -> Expr.Lambda(v, f body)
            | ShapeCombination (obj, exprs) -> RebuildShapeCombination(obj, exprs |> List.map f)

        f expr

    let patchMethodCall template replacer =
        patchMethodCallWithOther template <| fun receiver method args ->
                                                 let (newMethod, newArgs) = replacer method args
                                                 match receiver with
                                                 | Some r -> Expr.Call(r, newMethod, newArgs)
                                                 | None -> Expr.Call(newMethod, newArgs)


    // Returns the actual type to use for the given YANG type.
    // Tries to use pritive types as far as it can, otherwise (like in the case
    // of enums and unions) generates a new type.
    let generateLeafType (ctx: YANGProviderGenerationContext) (yangType: YANGType) =
        match yangType.PrimitiveType with
        | x when x = YANGPrimitiveTypes.Empty       -> typeof<unit>
        | x when x = YANGPrimitiveTypes.Boolean     -> typeof<bool>
        | x when x = YANGPrimitiveTypes.Int8        -> typeof<int8>
        | x when x = YANGPrimitiveTypes.Int16       -> typeof<int16>
        | x when x = YANGPrimitiveTypes.Int32       -> typeof<int32>
        | x when x = YANGPrimitiveTypes.Int64       -> typeof<int64>
        | x when x = YANGPrimitiveTypes.UInt8       -> typeof<uint8>
        | x when x = YANGPrimitiveTypes.UInt16      -> typeof<uint16>
        | x when x = YANGPrimitiveTypes.UInt32      -> typeof<uint32>
        | x when x = YANGPrimitiveTypes.UInt64      -> typeof<uint64>
        | x when x = YANGPrimitiveTypes.String      -> typeof<string>
        | x when x = YANGPrimitiveTypes.Binary      -> typeof<byte[]>
        | x when x = YANGPrimitiveTypes.Decimal64   -> typeof<double>
        | x when x = YANGPrimitiveTypes.Enumeration -> failwith "Enumerations not implemented."
        | x when x = YANGPrimitiveTypes.Union       -> failwith "Unions not implemented."
        | _ ->
            failwithf "Unexpected primitive type %A." yangType.PrimitiveType.Name
        
    /// Generetes a new type for container nodes.
    let rec generateContainerType (ctx: YANGProviderGenerationContext) (container: YANGDataNodeContainer) =
        
        // Crate a new type with the same name of the container.
        let newType = ProvidedTypeDefinition(normalizeName container.Name.Name, Some typeof<PathBuilder>, HideObjectMethods = true)
        
        // Recursively generate the types.
        ctx.Push(container, newType, ctx.CurrentPath.Append(container.Name.Name))
        generateCommonMembers ctx true
        container.DataNodes |> Seq.iter (generateTypesForNode ctx)
        ctx.Pop()
        
        // In case of a container, just add the type to the parent type,
        // but if we are dealing with a list, create an intermediary type that acts as a collection.
        match container with
        | :? YANGContainer ->
            
            ctx.CurrentType.AddMember(newType)
            newType

        | :? YANGList ->

            // Create a new collection type
            let collectionType = ProvidedTypeDefinition(newType.Name, Some typeof<PathBuilder>, HideObjectMethods = true)

            // Constructor
            collectionType.AddMember(
                ProvidedConstructor(
                    [ ProvidedParameter("obj", typeof<PathBuilder>) ],
                    InvokeCode = (fun args -> <@@ %%(args.[0]) : PathBuilder @@>)
                )
            )

            // Indexer property
            collectionType.AddMember(
                ProvidedProperty(
                    "Item",
                    newType,
                    [ ProvidedParameter("i", typeof<int>) ],
                    GetterCode = listIndexerGetter newType
                )
            )

            // Count property
            collectionType.AddMember(
                ProvidedProperty(
                    "Count",
                    typeof<int>,
                    GetterCode = (fun args -> <@@ (%%(args.[0]) : PathBuilder).GetListLength() @@>)
                )
            )

            // Adds the actual new type to the collection, and add the collection to the parent type in the context
            collectionType.AddMember(newType)
            ctx.CurrentType.AddMember(collectionType)
            collectionType

        | _ ->
            failwith "Unreachable"

    // Adds the common node members to the given type:
    // - A constructor accepting a CPSObject to use the given object as a backing store.
    // - A constructor requiring a CPSKey to create a backing CPSObject.
    // - Some other overloads for the last constructor.
    // - A CPSObject property to extract the underlying CPSObject.
    // - Some factory methods in the root type.
    and generateCommonMembers (ctx: YANGProviderGenerationContext) generateFactoryMethods =
        let ctor1 =
            ProvidedConstructor(
                [ ProvidedParameter("obj", typeof<PathBuilder>) ],
                InvokeCode = (fun args -> <@@ %%(args.[0]) : PathBuilder @@>)
            )
        let ctor2 =
            ProvidedConstructor(
                [ ProvidedParameter("path", typeof<CPSPath>) ],
                InvokeCode = (fun args -> <@@ PathBuilder.FromObject(CPSObject(%%(args.[0]) : CPSPath)) @@>)
            )
        
        let objProp =
            ProvidedMethod(
                "ToCPSObject",
                [],
                typeof<CPSObject>,
                InvokeCode = (fun args -> <@@ (%%(args.[0]) : PathBuilder).CPSObject @@>)
            )

        let t = ctx.CurrentType
        t.AddMembers([ ctor1; ctor2; ])
        t.AddMember(objProp)

        // Add shortcut methods to quickly execute transactions.
        let get =
            ProvidedMethod(
                "Get",
                [],
                typeof<Result<CPSObject list, string>>,
                InvokeCode = (fun args -> <@@ CPSTransaction.Get([ (%%(args.[0]) : PathBuilder).CPSObject ]) @@>)
            )
        let set =
            ProvidedMethod(
                "Set",
                [],
                typeof<Result<unit, string>>,
                InvokeCode = (fun args ->
                    <@@
                        let t = CPSTransaction()
                        t.Set((%%(args.[0]) : PathBuilder).CPSObject)
                        t.Commit()
                    @@>
                )
            )
        let create =
            ProvidedMethod(
                "Create",
                [],
                typeof<Result<unit, string>>,
                InvokeCode = (fun args ->
                    <@@
                        let t = CPSTransaction()
                        t.Create((%%(args.[0]) : PathBuilder).CPSObject)
                        t.Commit()
                    @@>
                )
            )
        let delete =
            ProvidedMethod(
                "Delete",
                [],
                typeof<Result<unit, string>>,
                InvokeCode = (fun args ->
                    <@@
                        let t = CPSTransaction()
                        t.Delete((%%(args.[0]) : PathBuilder).CPSObject)
                        t.Commit()
                    @@>
                )
            )
        t.AddMembers([ get; set; create; delete ])

        // Adds the corresponding factory methods to the root type
        if generateFactoryMethods then
            
            // We must be sure that the name we choose for the method is unique.
            let methodName =
                seq {
                    let mutable name = String.Empty
                    for t in ctx.TypesSequence do
                        name <- t.Name + name
                        yield name
                }
                |> Seq.find (fun name ->
                    ctx.RootType.GetMethods()
                    |> Array.exists (fun m -> m.Name = name)
                    |> not
                )

            let pathExpr = Expr.Value(ctx.CurrentPath.ToString(), typeof<string>)
            let keyExpr = Expr.Value(CPSKey(CPSQualifier.Target, ctx.CurrentPath).Key, typeof<string>)

            // The parameterless method constructs a new object
            let parameterlessFactoryMethod =
                ProvidedMethod(
                    methodName,
                    [],
                    t,
                    InvokeCode = (fun args -> Expr.NewObject(ctor2, [ <@@ CPSPath %%(pathExpr) @@> ])),
                    IsStaticMethod = true
                )

            // The other method takes an existing object and performs some validation
            let returnType = makeValidationResultType t
            let fromObjectFactoryMethod =
                ProvidedMethod(
                    methodName,
                    [ ProvidedParameter("obj", typeof<CPSObject>) ],
                    returnType,
                    InvokeCode = (fun args ->
                        <@@ YANGProviderRuntime.validateObject (%%(args.[0]) : CPSObject) %%(keyExpr) @@>
                    ),
                    IsStaticMethod = true
                )

            ctx.RootType.AddMembers [ parameterlessFactoryMethod; fromObjectFactoryMethod ]
        

    // Common getter for leaf data nodes.
    and leafPropertyGetter t (CPSAttributeID attrId) (args: Quotations.Expr list) =
        <@@
            // Adds the current path to the attribute segments and gets/sets the value
            (%%(args.[0]) : PathBuilder).Access(attrId).ReadLeaf<obj>()
        @@>

        // Ensure that the core method is called with the correct generic parameter
        |> patchMethodCall
               <@@ PathBuilder.Invalid.ReadLeaf() @@>
               (fun m args -> (ProvidedTypeBuilder.MakeGenericMethod(m.GetGenericMethodDefinition(), [ t ]), args))

    // Common setter for leaf data nodes.
    and leafPropertySetter t (CPSAttributeID attrId) (args: Quotations.Expr list) =
        <@@
            // Adds the current path to the attribute segments and gets/sets the value
            (%%(args.[0]) : PathBuilder).Access(attrId).WriteLeaf(None)
        @@>

        // Ensure that the core method is called with the correct generic parameter
        |> patchMethodCall
               <@@ PathBuilder.Invalid.WriteLeaf(None) @@>
               (fun m _ -> (ProvidedTypeBuilder.MakeGenericMethod(m.GetGenericMethodDefinition(), [ t ]), [ args.[1] ]))

    // Common getter for leaf-list nodes
    and leafListPropertyGetter t (CPSAttributeID attrId) (args: Quotations.Expr list) =
        <@@
            // Adds the current path to the attribute segments and gets/sets the value
            (%%(args.[0]) : PathBuilder).Access(attrId).ReadLeafList<obj>()
        @@>

        // Ensure that the core method is called with the correct generic parameter
        |> patchMethodCall
               <@@ PathBuilder.Invalid.ReadLeafList() @@>
               (fun m args -> (ProvidedTypeBuilder.MakeGenericMethod(m.GetGenericMethodDefinition(), [ t ]), args))

    // Common setter for leaf-list nodes
    and leafListPropertySetter t (CPSAttributeID attrId) (args: Quotations.Expr list) =
        <@@
            // Adds the current path to the attribute segments and gets/sets the value
            (%%(args.[0]) : PathBuilder).Access(attrId).WriteLeafList(None)
        @@>

        // Ensure that the core method is called with the correct generic parameter
        |> patchMethodCall
               <@@ PathBuilder.Invalid.WriteLeafList(None) @@>
               (fun m _ -> (ProvidedTypeBuilder.MakeGenericMethod(m.GetGenericMethodDefinition(), [ t ]), [ args.[1] ]))
            
    // Common getter for containers and lists that erases to an object construction.
    and constructorGetter (t: Type) currentAttrId (args: Quotations.Expr list) = 

        // Constructor for the container type
        let ctor =
            t.GetMembers(BindingFlags.Instance ||| BindingFlags.Public)
            |> Array.find (fun m ->
                m.MemberType = MemberTypes.Constructor && (
                    let pars = (m :?> ConstructorInfo).GetParameters()
                    pars.Length = 1 && pars.[0].ParameterType = typeof<PathBuilder>
                )
            )
            :?> ConstructorInfo
        
        // Constructs a new instance of the erased type passing the same object,
        // but adding a segment to the access path. In case no path has been provided, it is considered an indexer getter.
        match currentAttrId with
        | Some (CPSAttributeID attrId) ->
            <@@
                ignore ((%%(args.[0]) : PathBuilder).Access(attrId))
            @@>
        | None ->
            <@@
                ignore ((%%(args.[0]) : PathBuilder).Indexer((%%args.[1]) : int))
            @@>

        // Replace the call to `ignore` with the actual constructor call.
        |> patchMethodCallWithOther
               <@@ ignore @@>
               (fun _ _ args -> Expr.NewObject(ctor, args))

    // Common getter for container nodes
    and containerPropertyGetter t currentPath =
        constructorGetter t (Some currentPath)

    // Common getter for list nodes
    and listPropertyGetter t currentPath =
        constructorGetter t (Some currentPath)

    // Common getter for the indexer of a collection
    and listIndexerGetter t =
        constructorGetter t None


    // Adds new members to the given parent type from the given data node.
    // Adds new nested types for containers, lists and leaf-lists. For the leafs,
    // corresponding instance properties are also added.
    and generateTypesForNode (ctx: YANGProviderGenerationContext) (node: YANGDataNode) =
        let currentAttrId = ctx.CurrentPath.Append(node.Name.Name).AttributeID
                            |> Option.get
        match node with

        | :? YANGLeaf as leaf ->
            
            // Generates the type for the leaf
            let leafType = generateLeafType ctx leaf.Type.ResolvedType

            // Adds an instance property to the parent type for the leaf
            let prop =
                ProvidedProperty(
                    normalizeName leaf.Name.Name,
                    makeOptionType leafType,
                    GetterCode = leafPropertyGetter leafType currentAttrId,
                    SetterCode = leafPropertySetter leafType currentAttrId
                )
            if not (isNull leaf.Description) then
                prop.AddXmlDoc(leaf.Description)
            ctx.CurrentType.AddMember(prop)
        
        | :? YANGContainer as container ->
            
            // Generates a container type
            let containerType = generateContainerType ctx container
            
            // Adds an instance property to the parent type for the container
            let prop =
                ProvidedProperty(
                    normalizeName container.Name.Name,
                    containerType,
                    GetterCode = containerPropertyGetter containerType currentAttrId
                )
            if not (isNull container.Description) then
                prop.AddXmlDoc(container.Description)
            ctx.CurrentType.AddMember(prop)

        | :? YANGLeafList as leafList ->
            
            // Generates the type for the leaf
            let leafType = generateLeafType ctx leafList.Type.ResolvedType

            // Adds an instance property to the parent type for the leaf
            let prop =
                ProvidedProperty(
                    normalizeName leafList.Name.Name,
                    makeOptionType (makeListType leafType),
                    GetterCode = leafListPropertyGetter leafType currentAttrId,
                    SetterCode = leafListPropertySetter leafType currentAttrId
                )
            if not (isNull leafList.Description) then
                prop.AddXmlDoc(leafList.Description)
            ctx.CurrentType.AddMember(prop)
        
        | :? YANGList as list ->
            
            // Generates a container type
            let containerType = generateContainerType ctx list

            // Adds an instance property to the parent type for the list
            let prop =
                ProvidedProperty(
                    normalizeName list.Name.Name,
                    containerType,
                    GetterCode = listPropertyGetter containerType currentAttrId
                )
            if not (isNull list.Description) then
                prop.AddXmlDoc(list.Description)
            ctx.CurrentType.AddMember(prop)
        
        | _ -> ()


    // Generates the root type for a module, and recursively generates all the types for the inner nodes
    let generateRootTypeForModule typeName rootPath (m: YANGModule) =
        
        // Generates the root type
        let rootType =
            ProvidedTypeDefinition(asm, ns, typeName, Some typeof<PathBuilder>, HideObjectMethods = true)

        // Creates a new generation context
        let ctx = YANGProviderGenerationContext()
        ctx.Push(m, rootType, CPSPath(rootPath))

        // Generates common members for the root type
        generateCommonMembers ctx false |> ignore

        // Generates the subtypes from the other data nodes
        m.DataNodes |> Seq.iter (generateTypesForNode ctx)
        
        // Assert correct stack balance
        ctx.Pop()
        if not ctx.IsEmpty then
            failwith "YANGProviderGenerationContext is not balanced"

        //try
        //    Testing.FormatProvidedType(rootType, signatureOnly = false, ignoreOutput = false, useQualifiedNames = false)
        //    |> printf "%s\n"
        //with
        //| e -> printf "ERROR: %O" e

        rootType


    do
        yangProviderType.DefineStaticParameters(staticParams, (fun typeName args ->
            match args with
            | [| :? string as model; :? string as fileName; :? string as rootPath |] ->
        
                // Parser options
                let options = YANGParserOptions(
                                  // Ignore unknown statements
                                  UnknownStatement = fun _ -> Ok ()
                              )

                // Loads the model inline or from a file
                let m =
                    if not (String.IsNullOrEmpty(model)) then
                        YANGParser.ParseModule(model, options)
                    else if not (String.IsNullOrEmpty(fileName)) then
                        using (File.OpenRead(fileName)) (fun s -> YANGParser.ParseModule(s, options))
                    else
                        invalidOp "Please, provide a YANG model inline or specify the name of a file."

                // Parses the model string and creates the root type
                m
                |>> (fun m ->
                    let rootPath = if String.IsNullOrEmpty rootPath then m.Prefix else rootPath
                    generateRootTypeForModule typeName rootPath m
                )
                |> Result.mapError (List.map (fun e -> e.ToString()) >> String.concat String.Empty)
                |> Result.okOrThrow failwith

            | _ -> failwith "Unsupported static parameters."
        ))
        this.AddNamespace(ns, [ yangProviderType ])
