#!/usr/bin/env fsharpi

#I "../bin/Debug"
#r "FsCPS.dll"

open FsCPS
open FsCPS.TypeProviders

// Load the YANG model with the type provider
let [<Literal>] yangFileName = __SOURCE_DIRECTORY__ + "/example-model.yang"
type Example = YANGProvider<fileName = yangFileName>

let obj = Example.RootContainer()

// [ Access "example/root-container/container-leaf" ]
obj.ContainerLeaf <- Some 42

// [ Access "example/root-container/nested-container"; Access "example/root-container/nested-container/container-container-leaf" ]
obj.NestedContainer.ContainerContainerLeaf <- Some 42


let obj2 = Example.NestedContainer()

// [ Access "example/root-container/nested-container/container-container-leaf" ]
obj2.ContainerContainerLeaf <- Some 42