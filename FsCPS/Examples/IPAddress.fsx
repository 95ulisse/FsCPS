#I "../bin/Debug"
#r "FsCPS.dll"

open System.Net
open FsCPS

// These are the settings we are going to change
let ifindex = [| 2uy |]
let ipaddress = IPAddress.Parse("10.0.0.1").GetAddressBytes()
let ipaddress_prefix = [| 16uy |]

// Create a CPS object and set the attributes
let o = new CPSObject("base-ip/ipv4/address")
o.SetAttribute("ip", ipaddress)
o.SetAttribute("prefix-length", ipaddress_prefix)
o.SetAttribute(CPSPath "base-ip/ipv4/ifindex", ifindex)

// Create a new transaction
let trans = new CPSTransaction()
trans.Create(o)

// Commit the transaction
match trans.Commit() with
| Ok() -> printf "Ip address set!\n"
| Error e -> printf "Error: %s\n" e