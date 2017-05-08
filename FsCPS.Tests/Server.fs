namespace FsCPS.Tests

open FsCPS
open FsCPS.Tests.Utils
open Xunit

module Server =

    let serverKey = CPSKey "1.1222."
    let subkey1   = CPSKey "1.1222.1."
    let subkey2   = CPSKey "1.1222.2."
    let subkey3   = CPSKey "1.1222.3."
    
    let createAndInvoke transBuilder getHandler setHandler rollbackHandler =
        
        let mutable returnedObjects = []

        let server =
            {
                new ICPSServer with
                    member __.Get(obj) =
                        returnedObjects <-
                            getHandler obj
                            |> Seq.toList
                        Ok(returnedObjects :> seq<_>)
                    member __.Set(obj, reqType) =
                        setHandler obj reqType
                    member __.Rollback(obj) =
                        Ok(rollbackHandler obj)
            }

        use serverHandle =
            CPS.RegisterServer(serverKey, server)
            |> Result.okOrThrow invalidOp

        CPSTransaction.Get([ CPSObject(serverKey) ])
        |> Result.okOrThrow invalidOp
        |> (fun getResult ->
            getResult
            |> List.zip returnedObjects
            |> List.iter (fun (a, b) ->
                // The objects actually returned by the `getHandler` function
                // must be the same returned by the call to `CPSTransaction.Get`.
                AssertCPSObjectEquals a b
            )
        )
        
        let trans = CPSTransaction()
        transBuilder trans serverHandle.Key
        trans.Commit()
        |> Result.okOrThrow invalidOp

    [<Fact>]
    let ``Get requests return the same objects returned by the server`` () =
        let buildObjects k =
            let objWithAttr = CPSObject(subkey1)
            objWithAttr.SetAttribute(CPSPath "base-ip", [| 1uy; 2uy; 3uy; 4uy |])
            [
                CPSObject(subkey2);
                CPSObject(subkey3);
                objWithAttr
            ]

        // Tests with a server that only returns the precomputed objects.
        // The assertions are made by the `createAndInvoke` function.
        createAndInvoke
            (fun _ _ -> ())
            (fun obj -> buildObjects obj.Key.Key)
            (fun _ _ -> invalidOp "Set should not be called")
            (fun _ -> invalidOp "Rollback should not be called")
    
        
    [<Fact>]
    let ``Set method gets called`` () =
        let mutable operationsInvoked = []
        createAndInvoke
            (fun trans k ->
                trans.Create(CPSObject(k))
                trans.Set(CPSObject(k))
                trans.Delete(CPSObject(k))
            )
            (fun _ -> [])
            (fun obj opType ->
                operationsInvoked <- opType :: operationsInvoked
                Ok(obj)
            )
            (fun _ -> invalidOp "Rollback should not be called")
        
        Assert.Equal<CPSOperationType>(
            [ CPSOperationType.Delete; CPSOperationType.Set; CPSOperationType.Create ],
            operationsInvoked
        )
            
    [<Fact>]
    let ``Rollback method gets passed the object returned by the set method`` () =
        let mutable rollbackObject = Unchecked.defaultof<CPSObject>
        createAndInvoke
            (fun trans k ->
                let objKey = CPSKey(k.Key + "100.")
                trans.Create(CPSObject(objKey))
                trans.Delete(CPSObject(objKey))
            )
            (fun _ -> [])
            (fun obj opType ->
                match opType with
                | CPSOperationType.Create ->
                    let rollObj = CPSObject(obj.Key)
                    rollbackObject <- rollObj
                    Ok rollObj
                | CPSOperationType.Delete ->
                    Error ""
                | _ ->
                    invalidOp "Unexpected operation."
            )
            (fun obj ->
                Assert.Equal(rollbackObject.Key.Key, obj.Key.Key)
            )