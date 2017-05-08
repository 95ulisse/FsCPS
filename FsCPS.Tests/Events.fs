namespace FsCPS.Tests

open System
open System.Threading
open FsCPS
open Xunit

module Events =
    
    let publishAndListen f (errListener: _ option) (completedListener: _ option) =
        let obs =
            CPS.ListenEvents(CPSKey "1.1099.")
            |> Result.okOrThrow invalidOp

        // The countdown will be used to be sure that the passed functions
        // are called at least once
        use countdown =
            new CountdownEvent(
                1 +
                (if errListener.IsSome then 1 else 0) +
                (if completedListener.IsSome then 1 else 0)
            )
        let mutable result = None

        obs.Subscribe(
            {
                new IObserver<_> with
                    member __.OnNext(obj) =
                        try
                            result <- Some (f obj)
                        finally
                            countdown.Signal() |> ignore
                    member __.OnError(e) =
                        match errListener with
                        | Some listener ->
                            result <- Some (listener e)
                            countdown.Signal() |> ignore
                        | None -> ()
                    member __.OnCompleted() =
                        match completedListener with
                        | Some listener ->
                            listener ()
                            countdown.Signal() |> ignore
                        | None -> ()
            }
        ) |> ignore

        // Publish an event to trigger the observable
        CPS.PublishEvent(CPSObject(CPSKey "1.1099."))
        |> Result.okOrThrow invalidOp

        // Wait 1s for the countdown to reach 0, otherwise throw an exception.
        // If a listener for the completed event has been passed, dispose the observable
        // and wait another second before throwing.
        if not (countdown.Wait(1000)) then
            if completedListener.IsSome then
                obs.Dispose()
                if not (countdown.Wait(1000)) then
                    invalidOp "Event timeout"
            else
                invalidOp "Event timeout"

        if not obs.IsCompleted then
            obs.Dispose()

        result.Value


    [<Fact>]
    let ``Observable events are fired on another thread`` () =
        let otherThreadId = publishAndListen (fun _ -> Thread.CurrentThread.ManagedThreadId) None None
        Assert.NotEqual(Thread.CurrentThread.ManagedThreadId, otherThreadId)

    [<Fact>]
    let ``An exception in a listener is caught and converted to an error signal`` () =
        Assert.True(
            publishAndListen
                (fun _ -> invalidOp "A bad exception"; false)
                (Some (fun _ -> true))
                None
        )

    [<Fact>]
    let ``When the observable is disposed, raises a completed signal`` () =
        let mutable hasCompleted = false
        Assert.True(
            publishAndListen
                (fun _ -> true)
                None
                (Some (fun _ -> hasCompleted <- true))
        )
        Assert.True(hasCompleted)