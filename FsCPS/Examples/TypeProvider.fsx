#!/usr/bin/env fsharpi

//#I "../bin/Debug"
//#r "FsCPS.dll"

open System
open System.Net
open FsCPS

// A simplified version of the dell-base-ip model
type BaseIP = YANGTypeProvider<"""
    module dell-base-ip {
        namespace "http://www.dell.com/esg/networking/os10/dell-base-ip";
        prefix "base-ip";

        organization "Dell Inc";

        contact "http://www.dell.com/support/softwarecontacts";

        description "This model will support the configruation of IP address.
    	    ***Depreciated****
    	    ";

        revision 2015-08-12 {
            description "Initial version.";
        }

        typedef logical-ifindex {
            type uint32;
            description
                "Uniquely identifies any interface system-wide including
                physical ports, LAG interfaces and Vlan interfaces";
        }

        typedef ipv4-address {
            type binary {
                length "4";
            }
            description
                "This type denotes a IPv4 address as a 4 octet binary number
                in network-byte order.";
        }

        typedef ipv6-address {
            type binary {
                length "16";
            }
            description
                "This type denotes a IPv6 address as a 16 octet binary number
                in network-byte order.";
        }

        typedef ip-address {
            type union {
                type ipv4-address;
                type ipv6-address;
            }
            description
                "This type denotes a IP address (V4 or V6) 4/16 binary number
                in network-byte order.";
        }

        list ipv4 {
            leaf ifindex {
	     	    type logical-ifindex;
	      	    description "The interface index";
	        }
		    leaf enabled {
			    type boolean;
			    description "This is true if IPv4 is enabled.";
		    }
		    leaf forwarding {
			    type boolean;
			    description
                    "This will be set true if the system has IP forwarding enabled.
			        This will only set the kernel IP forwarding flags.";			
		    }
		    leaf vrf-id {
			    type uint32;
			    description "A numerical value of the vrf that contains the interface.  Use 0 for the default.";
		    }
		    leaf name {
			    type string;
			    description "The interfaces's name.";
		    }

            list address {		
		        leaf ip {
		      	    type ip-address;
		      	    description "IP address";
		        }
		        leaf prefix-length {
		    	    type uint8;
		        }
		     			
		    }		
	    }
    }
""">

// Gets the address of the first interface
let address =
    CPSObject "base-ip/ipv4/address"
    |> Array.create 1
    |> CPSTransaction.Get
    |> function
       | Error e -> raise (Exception e)
       | Ok x -> x
    |> List.head

// Using the type provider, create a typed view over the address
let a = BaseIP.Ipv4(address)
printf "Interface index: %d\n" a.Ifindex.Some
printf "Addresses:\n"
a.Addresses |> Seq.iter (fun addr ->
    let addrStr =
        match addr.Ip with
        | Some Ipv4Address x -> sprintf "- IPv4: %s" (IPAddress(x).ToString())
        | Some Ipv6Address x -> sprintf "- IPv6: %s" (IPAddress(x).ToString())
        | _ -> raise (Exception "Unexpected value")
    printf "%s/%d\n" addrStr addr.PrefixLength
)

printf "\n\n"

// Note that the provided types are just a view over the same data.
// This is equivalent to the previous code.
let a = BaseIP.Ipv4.Address(address)
let addrStr =
    match a.Ip with
    | Some Ipv4Address x -> sprintf "IPv4: %s" (IPAddress(x).ToString())
    | Some Ipv6Address x -> sprintf "IPv6: %s" (IPAddress(x).ToString())
    | _ -> raise (Exception "Unexpected value")
printf "%s/%d\n" addrStr a.PrefixLength

printf "\n\n"

// We can also use the same providers to build CPS objects.
// Note that `a` carries data as an actual CPS object because we instantiated
// it with a key, while `addr` is just a temporary container for the data.
let a = BaseIP.Ipv4("base-ip/ipv4/address")
let addr =
    BaseIP.Ipv4.Address(
        Ip = Some (IPAddress.Parse("10.0.0.2").GetAddressBytes()),
        PrefixLength = Some 16uy
    )
a.Ifindex <- Some 40uy
a.Addresses.Add addr
let newAddressObject = a.ToCPSObject()

// Print the contents of the object to be sure that they are correct.
printf "Key: %s\n" newAddressObject.Key.Key
newAddressObject.Attributes.Values |> Seq.iter (fun attr ->
    printf "%s: %A\n" (attr.Path.ToString()) attr.Value
)
printf "\n"

// We created a new ip address, what should we do with it?
let trans = CPSTransaction()
trans.Set newAddressObject
match trans.Commit() with
| Ok () -> printf "Address added!\n"
| Error e -> raise (Exception e)