#!/usr/bin/env fsharpi

#I "../bin/Debug"
#r "FsCPS.dll"

open FsCPS

let argv = System.Environment.GetCommandLineArgs()
if argv.Length < 4 then
    printf "Usage: %s key\n" argv.[2]
    exit 1

let obs =
    match CPS.ListenEvents(CPSKey(argv.[3])) with
    | Ok obs -> obs
    | Error e -> invalidOp (sprintf "Error listening for events: %s\n" e)

obs
|> Observable.subscribe (fun obj ->
    printf "Received object! Key: %s\n" obj.Key.Key
)

printf "Press enter to stop..."
System.Console.ReadLine()

obs.Dispose()