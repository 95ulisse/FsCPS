#!/usr/bin/env fsharpi

#I "../bin/Debug"
#r "FsCPS.dll"

open FsCPS
open FsCPS.TypeProviders

// Load the YANG model with the type provider
let [<Literal>] yangFileName = __SOURCE_DIRECTORY__ + "/dell-base-vlan.yang"
type VLANEntry = YANGProvider<fileName = yangFileName>

let obj = VLANEntry(CPSPath "base-vlan/entry")
obj.Id <- Some 43us
obj.UntaggedPorts <- Some [ 41u ]
printf "%s\n" (obj.ToString(true))