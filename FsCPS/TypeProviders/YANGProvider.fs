namespace ProviderImplementation

#nowarn "10001"

open System
open System.Collections.Generic
open System.Reflection
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open ProviderImplementation
open ProviderImplementation.ProvidedTypes
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


[<TypeProvider>]
type YANGProvider(config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()

    let ns = "FsCPS.TypeProviders"
    let asm = Assembly.GetExecutingAssembly()
    let factory = ProvidedTypesContext.Create(config)

    let yangProviderType = factory.ProvidedTypeDefinition(asm, ns, "YANGProvider", None, hideObjectMethods = true, nonNullable = true)
    let staticParams =
        [
            ProvidedStaticParameter("rootPath", typeof<string>);
            ProvidedStaticParameter("model", typeof<string>)
        ]

    let nameNormalizerRegex = Regex("(?:^|-|\.)(\w)", RegexOptions.Compiled)


    // Normalizes the name of a YANG node to be suitable as a standard .NET identifier.
    let normalizeName (name: string) =
        nameNormalizerRegex.Replace(name, fun m -> m.Groups.[1].Value.ToUpperInvariant())
            .Trim([| '-'; '.' |])

    let makeOptionType t =
        typedefof<option<_>>.MakeGenericType([| t |])


    // Returns the actual type to use for the given YANG type.
    // Tries to use pritive types as far as it can, otherwise (like in the case
    // of enums and unions) generates a new type.
    let generateType (ctx: YANGProviderGenerationContext) (yangType: YANGType) =
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

    // Adds the common node constructors to the given type:
    // - A constructor accepting a CPSObject to use the given object as a backing store.
    // - A constructor requiring a CPSKey to create a backing CPSObject.
    // - Some other overloads for the last constructor.
    let generateCommonConstructors (t: ProvidedTypeDefinition) =
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
                [ factory.ProvidedParameter("pathStr", typeof<string>) ],
                (fun args -> <@@ CPSObject(%%(args.[0]) : string) @@>)
            )
        let ctor5 =
            factory.ProvidedConstructor(
                [
                    factory.ProvidedParameter("path", typeof<CPSPath>);
                    factory.ProvidedParameter("qual", typeof<CPSQualifier>)
                ],
                (fun args -> <@@ CPSObject((%%(args.[0]) : CPSPath), (%%(args.[1]) : CPSQualifier)) @@>)
            )

        t.AddMembers([ ctor1; ctor2; ctor3; ctor4; ctor5 ])
        t

    // Common getter for leaf data nodes.
    let leafPropertyGetter t path (args: Quotations.Expr list) =
        let expr = <@@ readAttribute<obj> (CPSPath path) (%%(args.[0]) : CPSObject) @@>
        
        // Be sure that we call the `readAttribute` method with the correct generic parameter
        match expr with
        | Call(None, method, args) ->
            Expr.Call(method.GetGenericMethodDefinition().MakeGenericMethod([| t |]), args)
        | _ ->
            failwith "Should never be reached"

    // Common setter for leaf data nodes.
    let leafPropertySetter t path args =
        <@@ failwith "Not implemented." @@>

    // Adds new members to the given parent type from the given data node.
    // Adds new nested types for containers, lists and leaf-lists. For the leafs,
    // corresponding instance properties are also added.
    let generateTypes (ctx: YANGProviderGenerationContext) (node: YANGDataNode) =
        match node with

        | :? YANGLeaf as leaf ->
            
            // Generates the type for the leaf
            let leafType = generateType ctx leaf.Type.ResolvedType

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
            failwith "YANGContainer not implemented."
        
        | :? YANGList as list ->
            failwith "YANGList not implemented."
        
        | :? YANGLeafList as leafList ->
            failwith "YANGLeafList not implemented."
        
        | _ -> ()

    // Generates the root type for a module, and recursively generates all the types for the inner nodes
    let generateRootTypeForModule typeName rootPath (m: YANGModule) =
        
        // Generates the root type
        let rootType =
            factory.ProvidedTypeDefinition(asm, ns, typeName, Some typeof<CPSObject>, hideObjectMethods = true, nonNullable = true)
            |> generateCommonConstructors

        // Creates a new generation context
        let ctx = YANGProviderGenerationContext()
        ctx.Push(m, rootType, CPSPath(rootPath))

        // Generates the subtypes from the other data nodes
        m.DataNodes |> Seq.iter (generateTypes ctx)

        Testing.FormatProvidedType(rootType, signatureOnly = false, ignoreOutput = false, useQualifiedNames = false)
        |> printf "%s\n"

        rootType


    do
        yangProviderType.DefineStaticParameters(staticParams, (fun typeName args ->
            match args with
            | [| :? string as rootPath; :? string as model |] ->
        
                // Parses the model string and creates the root type
                YANGParser.ParseModule(model)
                |>> generateRootTypeForModule typeName rootPath
                |> Result.mapError (List.map (fun e -> e.ToString()) >> String.concat String.Empty)
                |> Result.okOrThrow failwith

            | _ -> failwith "Unsupported static parameters."
        ))
        this.AddNamespace(ns, [yangProviderType])