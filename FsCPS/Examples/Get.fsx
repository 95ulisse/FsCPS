#!/usr/bin/env fsharpi

#I "../bin/Debug"
#r "FsCPS.dll"

open FsCPS

let argv = System.Environment.GetCommandLineArgs()
if argv.Length < 4 then
    printf "Usage: %s key\n" argv.[2]
    exit 1

let obj = CPSObject(CPSPath (argv.[3]))

match CPSTransaction.Get([ obj ]) with
| Ok _ -> printf "Get succeeded!\n"
| Error e -> printf "Error: %s\n" e
