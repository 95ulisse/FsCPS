#!/usr/bin/env fsharpi

#I "../bin/Debug"
#r "FsCPS.dll"

open FsCPS

let argv = System.Environment.GetCommandLineArgs()
if argv.Length < 4 then
    printf "Usage: %s key\n" argv.[2]
    exit 1

// Builds the object that will be used as a filter for the get request.
let obj = CPSObject(CPSPath (argv.[3]))

// Sends the request and prints the results.
match CPSTransaction.Get([ obj ]) with
| Error e ->
    printf "Error: %s\n" e
| Ok objects ->
    objects
    |> Seq.map (fun o -> o.ToString(true))
    |> Seq.iter (printf "%s\n\n")
