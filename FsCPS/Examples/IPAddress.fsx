#I "../bin/Debug"
#r "FsCPS.dll"

open System.Net
open FsCPS

// If the arg "--delete" is passed to the script,
// do remove the address instead of adding it.
let argv = System.Environment.GetCommandLineArgs()
let doDelete =
    if argv.Length >= 4 then
        if argv.[3] = "--delete" then
            true
        else
            printf "The only allowed option is --delete to remove the address.\n"
            exit 1
    else
        false

// These are the settings we are going to change
let ifindex = [| 40uy |]
let ipaddress = IPAddress.Parse("10.0.0.1").GetAddressBytes()
let ipaddress_prefix = [| 16uy |]

// Create a CPS object and set the attributes
let o = new CPSObject("base-ip/ipv4/address")
o.SetAttribute("ip", ipaddress)
o.SetAttribute("prefix-length", ipaddress_prefix)
o.SetAttribute(CPSPath "base-ip/ipv4/ifindex", ifindex)

// Create a new transaction
let trans = new CPSTransaction()
if doDelete then
    trans.Delete(o)
else
    trans.Create(o)

// Commit the transaction
match trans.Commit() with
| Ok() -> if doDelete then printf "Address deleted!\n" else printf "Address set!\n"
| Error e -> printf "Error: %s\n" e