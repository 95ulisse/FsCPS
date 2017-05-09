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
| Error e -> printf "Error: %s\n" e
| Ok objects ->
    objects |> List.iter (fun o ->
        printf "Key: %s\n" o.Key.Key
        o.Attributes.Values |> Seq.iter (fun attr ->
            printf "%s: %A\n" (attr.Path.ToString()) attr.Value
        )
        printf "\n"
    )