#!/usr/bin/env fsharpi

#I "../bin/Debug"
#r "FsCPS.dll"

open System
open FsCPS

// Checks that the key is present.
let argv = System.Environment.GetCommandLineArgs()
if argv.Length < 4 then
    printf "Usage: %s key\n" argv.[2]
    exit 1
let key = CPSKey(CPSQualifier.Target, CPSPath(argv.[3]))

// Creates the observable that will react to the events.
let obs =
    match CPS.ListenEvents(key) with
    | Ok obs -> obs
    | Error e -> invalidOp (sprintf "Error listening for events: %s\n" e)

// Subscribes to the observable.
obs |> Observable.subscribe (fun evt ->
    printf "Operation: %s\nKey: %s\n" (Enum.GetName(typeof<CPSOperationType>, evt.Operation)) evt.Object.Key.Key
    evt.Object.Attributes.Values |> Seq.iter (fun attr ->
        printf "%s: %A\n" (attr.Path.ToString()) attr.Value
    )
    printf "\n"
)

// Pause
printf "Registered for key %s\n" key.Key
printf "Press enter to stop...\n"
System.Console.ReadLine()

obs.Dispose()