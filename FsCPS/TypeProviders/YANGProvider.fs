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
open ProviderImplementation
open ProviderImplementation.ProvidedTypes
open ProviderImplementation.ProvidedTypes.UncheckedQuotations
open FsCPS
open FsCPS.Yang
open FsCPS.TypeProviders.Runtime.YANGProviderRuntime

#if DEBUG
open ProviderImplementation.ProvidedTypesTesting
#endif


type internal YANGProviderGenerationContext() =
    let nodeStack = Stack<YANGNode>()
    let typeStack = Stack<ProvidedTypeDefinition>()
    let pathStack = Stack<CPSPath>()

    member this.Push(node, t, path) =
        nodeStack.Push(node)
        typeStack.Push(t)
        pathStack.Push(path)

    member this.Pop() =
        nodeStack.Pop() |> ignore
        typeStack.Pop() |> ignore
        pathStack.Pop() |> ignore

    member this.CurrentNode
        with get() = nodeStack.Peek()

    member this.CurrentType
        with get() = typeStack.Peek()

    member this.CurrentPath
        with get() = pathStack.Peek()
        
    member this.IsEmpty
        with get() = nodeStack.Count = 0


[<TypeProvider>]
type YANGProvider(config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()

    let ns = "FsCPS.TypeProviders"
    let asm = Assembly.GetExecutingAssembly()
    let factory = ProvidedTypesContext.Create(config)

    let yangProviderType = factory.ProvidedTypeDefinition(asm, ns, "YANGProvider", None, hideObjectMethods = true, nonNullable = true)
    let staticParams =
        [
            ProvidedStaticParameter("model", typeof<string>);
            ProvidedStaticParameter("fileName", typeof<string>, "");
            ProvidedStaticParameter("rootPath", typeof<string>, "");
        ]

    let nameNormalizerRegex = Regex("(?:^|-|\.)(\w)", RegexOptions.Compiled)


    // Normalizes the name of a YANG node to be suitable as a standard .NET identifier.
    let normalizeName (name: string) =
        nameNormalizerRegex.Replace(name, fun m -> m.Groups.[1].Value.ToUpperInvariant())
            .Trim([| '-'; '.' |])

    let makeOptionType t =
        typedefof<option<_>>.MakeGenericType([| t |])

    let makeListType t =
        typedefof<list<_>>.MakeGenericType([| t |])


    // Returns the actual type to use for the given YANG type.
    // Tries to use pritive types as far as it can, otherwise (like in the case
    // of enums and unions) generates a new type.
    let generateLeafType (ctx: YANGProviderGenerationContext) (yangType: YANGType) =
        match yangType.PrimitiveType with
        | x when x = YANGPrimitiveTypes.Empty       -> failwith "Invalid type Empty."
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
        
        // Crate a new type with the same name of the container and add it to the parent type
        let newType =
            factory.ProvidedTypeDefinition(
                normalizeName container.Name.Name,
                Some typeof<CPSObject>,
                hideObjectMethods = true,
                nonNullable = true
            )
            |> generateCommonMembers
        ctx.CurrentType.AddMember(newType)
        
        // Recursively generate the types 
        ctx.Push(container, newType, ctx.CurrentPath.Append(container.Name.Name))
        container.DataNodes |> Seq.iter (generateTypesForNode ctx)
        ctx.Pop()
        
        newType
        

    // Adds the common node members to the given type:
    // - A constructor accepting a CPSObject to use the given object as a backing store.
    // - A constructor requiring a CPSKey to create a backing CPSObject.
    // - Some other overloads for the last constructor.
    // - A CPSObject property to extract the underlying CPSObject.
    and generateCommonMembers (t: ProvidedTypeDefinition) =
        let ctor1 =
            factory.ProvidedConstructor(
                [ factory.ProvidedParameter("obj", typeof<CPSObject>) ],
                (fun args -> <@@ %%(args.[0]) : CPSObject @@>)
            )
        let ctor2 =
            factory.ProvidedConstructor(
                [ factory.ProvidedParameter("key", typeof<CPSKey>) ],
                (fun args -> <@@ CPSObject(%%(args.[0]) : CPSKey) @@>)
            )
        let ctor3 =
            factory.ProvidedConstructor(
                [ factory.ProvidedParameter("path", typeof<CPSPath>) ],
                (fun args -> <@@ CPSObject(%%(args.[0]) : CPSPath) @@>)
            )
        let ctor4 =
            factory.ProvidedConstructor(
                [
                    factory.ProvidedParameter("path", typeof<CPSPath>);
                    factory.ProvidedParameter("qual", typeof<CPSQualifier>)
                ],
                (fun args -> <@@ CPSObject((%%(args.[0]) : CPSPath), (%%(args.[1]) : CPSQualifier)) @@>)
            )
        
        let objProp =
            factory.ProvidedMethod(
                "ToCPSObject",
                [],
                typeof<CPSObject>,
                (fun args -> <@@ %%(args.[0]) : CPSObject @@>),
                isStatic = false
            )

        t.AddMembers([ ctor1; ctor2; ctor3; ctor4 ])
        t.AddMember(objProp)
        t
        

    // Common getter for leaf data nodes.
    and leafPropertyGetter t path (args: Quotations.Expr list) =
        let expr = <@@ readAttribute<obj> (CPSPath path) (%%(args.[0]) : CPSObject) @@>
        
        // Be sure that we call the `readAttribute` method with the correct generic parameter
        match expr with
        | Call(None, mtd, args) ->
            Expr.Call(mtd.GetGenericMethodDefinition().MakeGenericMethod([| t |]), args)
        | _ ->
            failwith "Should never be reached"

    // Common setter for leaf data nodes.
    and leafPropertySetter t path (args: Quotations.Expr list) =
        let expr = <@@ writeAttribute<obj> (CPSPath path) None (%%(args.[0]) : CPSObject) @@>
        
        // Be sure that we call the `writeAttribute` method with the correct generic parameter
        match expr with
        | Call(None, mtd, [ arg1; _; arg3 ]) ->
            Expr.Call(mtd.GetGenericMethodDefinition().MakeGenericMethod([| t |]), [ arg1; args.[1]; arg3 ])
        | _ ->
            failwith "Should never be reached"
            
    // Common getter for container nodes
    and containerPropertyGetter (t: ProvidedTypeDefinition) (args: Quotations.Expr list) =
        // This is a manual construction for:
        // <@@ new t(args.[0] : CPSObject) @@>
        let ctor =
            t.GetMembers(BindingFlags.Instance ||| BindingFlags.Public)
            |> Array.find (fun m ->
                m.MemberType = MemberTypes.Constructor && (
                    let pars = (m :?> ConstructorInfo).GetParameters()
                    pars.Length = 1 && pars.[0].ParameterType.Name.Contains(typeof<CPSObject>.Name)
                ) 
            )
        Expr.NewObjectUnchecked(ctor :?> ConstructorInfo, [ args.[0] ])

    // Common getter for list nodes
    and listPropertyGetter (t: ProvidedTypeDefinition) (args: Quotations.Expr list) =
        <@@ raise (NotImplementedException "List getters not implemented yet.") @@>

    // Common setter for list nodes
    and listPropertySetter (t: ProvidedTypeDefinition) (args: Quotations.Expr list) =
        <@@ raise (NotImplementedException "List setters not implemented yet.") @@>


    // Adds new members to the given parent type from the given data node.
    // Adds new nested types for containers, lists and leaf-lists. For the leafs,
    // corresponding instance properties are also added.
    and generateTypesForNode (ctx: YANGProviderGenerationContext) (node: YANGDataNode) =
        match node with

        | :? YANGLeaf as leaf ->
            
            // Generates the type for the leaf
            let leafType = generateLeafType ctx leaf.Type.ResolvedType

            // Adds an instance property to the parent type for the leaf
            let prop =
                factory.ProvidedProperty(
                    normalizeName leaf.Name.Name,
                    makeOptionType leafType,
                    leafPropertyGetter leafType (ctx.CurrentPath.Append(leaf.Name.Name).ToString()),
                    leafPropertySetter leafType (ctx.CurrentPath.Append(leaf.Name.Name).ToString())
                )
            if not (isNull leaf.Description) then
                prop.AddXmlDoc(leaf.Description)
            ctx.CurrentType.AddMember(prop)
        
        | :? YANGContainer as container ->
            
            // Generates a container type
            let containerType = generateContainerType ctx container
            
            // Adds an instance property to the parent type for the container
            let prop =
                factory.ProvidedProperty(
                    normalizeName container.Name.Name,
                    containerType,
                    containerPropertyGetter containerType
                )
            if not (isNull container.Description) then
                prop.AddXmlDoc(container.Description)
            ctx.CurrentType.AddMember(prop)
            
        
        | :? YANGList as list ->
            
            // Generates a container type
            let containerType = generateContainerType ctx list

            // Adds an instance property to the parent type for the list
            let prop =
                factory.ProvidedProperty(
                    normalizeName list.Name.Name,
                    makeListType containerType,
                    listPropertyGetter containerType,
                    listPropertySetter containerType
                )
            if not (isNull list.Description) then
                prop.AddXmlDoc(list.Description)
            ctx.CurrentType.AddMember(prop)
        
        | :? YANGLeafList as leafList ->
            failwith "YANGLeafList not implemented."
        
        | _ -> ()


    // Generates the root type for a module, and recursively generates all the types for the inner nodes
    let generateRootTypeForModule typeName rootPath (m: YANGModule) =
        
        // Generates the root type
        let rootType =
            factory.ProvidedTypeDefinition(asm, ns, typeName, Some typeof<CPSObject>, hideObjectMethods = true, nonNullable = true)
            |> generateCommonMembers

        // Creates a new generation context
        let ctx = YANGProviderGenerationContext()
        ctx.Push(m, rootType, CPSPath(rootPath))

        // Generates the subtypes from the other data nodes
        m.DataNodes |> Seq.iter (generateTypesForNode ctx)
        
        // Assert correct stack balance
        ctx.Pop()
        if not ctx.IsEmpty then
            failwith "YANGProviderGenerationContext is not balanced"

        Testing.FormatProvidedType(rootType, signatureOnly = false, ignoreOutput = false, useQualifiedNames = false)
        |> printf "%s\n"

        rootType


    do
        yangProviderType.DefineStaticParameters(staticParams, (fun typeName args ->
            match args with
            | [| :? string as model; :? string as fileName; :? string as rootPath |] ->
        
                // Loads the model inline or from a file
                let m =
                    if not (String.IsNullOrEmpty(model)) then
                        YANGParser.ParseModule(model)
                    else if not (String.IsNullOrEmpty(fileName)) then
                        using (File.OpenRead(fileName)) YANGParser.ParseModule
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
        this.AddNamespace(ns, [yangProviderType])
