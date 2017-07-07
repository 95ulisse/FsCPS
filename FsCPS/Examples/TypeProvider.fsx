#!/usr/bin/env fsharpi

#I "../bin/Debug"
#r "FsCPS.dll"

open System
open System.Net
open FsCPS
open FsCPS.TypeProviders

// A simplified version of the dell-base-ip model
type Simple = YANGProvider<"""
    module simple {
        namespace "http://example.com";
        prefix simple;

        leaf simple-value {
            type uint32;
        }
    }
""">

let obj = Simple(CPSPath "simple")
obj.SimpleValue <- Some 42u
printf "%d\n" obj.SimpleValue.Value