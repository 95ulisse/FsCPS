# Getting started

FsCPS exposes two main interfaces: a raw untyped interface similar to the official Python APIs,
and a type provider which uses the YANG models to create type-safe wrappers.
We will present the untyped API first, and then we will perform the same operations with the type
provider. Note that in the future, when the provider will be complete, it will be the preferred
method.

As an example, let's write together some F# scripts to get, add and remove IP addresses to interfaces.



## Untyped API

The first thing to do is load FsCPS and open the namespace:

```fsharp
#I "path/to/fscps/directory"
#r "FsCPS.dll"

open FsCPS
```

Let's say that we want to list the addresses assigned to the interface with index 40.

```fsharp
// Prepare the object
let obj = CPSObject(CPSPath "base-ip/ipv4/address")
obj.SetAttribute(CPSPath "base-ip/ipv4/ifindex", [| 40uy |])

// Execute the Get operation
match CPSTransaction.Get([ obj ]) with
| Error e ->
    printf "Error: %s\n" e
| Ok res ->
    // Extract and print IP address and prefix length
    res
    |> List.map (fun o -> (o.GetLeaf(CPSPath "base-ip/ipv4/address/ip").Value,
                           o.GetLeaf(CPSPath "base-ip/ipv4/address/prefix-length").Value))
    |> List.iter (fun (ip, prefix) ->
        printf "%s/%d\n" (System.Net.IPAddress(ip).ToString()) (byte prefix.[0])
    )
```

Let's break it down:

- First of all, we create a new `CPSObject` passing the key to the constructor.
  Here we omitted the qualifier (which defaults to `Target`), but there are other overloads that
  accept a qualifier.
- Then we set the attribute `base-ip/ipv4/ifindex` to indicate that we are referring to the interface
  with index 40.
- With `CPSTransaction.Get` we perform the operation. Note that this method is **blocking**, which means
  that it will return only when the results are available.
- All the operations that may fail return a `Result<_, _>`, and here we check with the `match` that
  the `get` operation completed successfully.
- If the operation completed successfully, we get back a list of objects containing the information
  we requested. Using the method `GetLeaf` we extract the IP address and the prefix length, and then
  we print them. Note that the values we extract are only raw byte arrays.

An example result of running the previous snippet is:

```
$ fsharpi example.fsx
10.0.0.1/16
10.1.0.1/16
```

Let's say now that we want to add a new address (10.2.0.1/24) to the same interface.

```fsharp
// Prepare the object
let obj = CPSObject(CPSPath "base-ip/ipv4/address")
obj.SetAttribute("ip", [| 10uy; 2uy; 0uy; 1uy |])
obj.SetAttribute("prefix-length", [| 24uy |])
obj.SetAttribute(CPSPath "base-ip/ipv4/ifindex", [| 40uy |])

// Create a new transaction
let trans = CPSTransaction()
trans.Create(obj)

// Commit the transaction
match trans.Commit() with
| Ok () -> printf "Address set!\n"
| Error e -> printf "Error: %s\n" e
```

In the first place, we prepare an object containing all the information needed to assign the address,
then we create a new **transaction** in which we add the object we just prepared with the `Create` method.
In a transaction we can include more than one object with more than one operation type: there is
a method for every operation type `Set`, `Create` and `Delete`. Note that these methods *do not*
execute the transaction: use the `Commit` method to finalize the transaction and actually execute it.
Again, remember that `Commit` is **blocking**, and will return only after it completed.

We can confirm that the script worker by checking with `iproute2`:

```
$ ip a
[...]
40: e101-026-0: <BROADCAST,MULTICAST> mtu 1500 qdisc noop master br100 state DOWN group default qlen 500
    link/ether ec:f4:bb:fb:d5:d6 brd ff:ff:ff:ff:ff:ff
    inet 10.0.0.1/16 scope global e101-026-0
       valid_lft forever preferred_lft forever
    inet 10.1.0.1/16 scope global e101-026-0
       valid_lft forever preferred_lft forever
    inet 10.2.0.1/24 scope global e101-026-0
       valid_lft forever preferred_lft forever
[...]
```

To summarize, some key points to note:

- The attributes of an object can be arbitrarily nested using containers and lists.
- The only attributes that can hold actual values are leafs and leaf-lists, and those values
  are just raw byte arrays.
- Transactions are populated using the `Set`, `Create` and `Delete` methods, and committed with `Commit`.



## Using the type provider

Let's see how can we use the type provider to accomplish the same goal, but with a much friendlier API.

First of all, load the YANG model of your interest with the `YANGProvider` type provider:

```fsharp
open FsCPS.TypeProviders

type IP = YANGProvider<fileName = "/opt/dell/os10/share/yang-models/dell-base-ip.yang">
```

We loaded the `dell-base-ip` model because we wanted to manipulate IP addresses.
Note also that the full path to the YANG model may vary (for reference, there's an
[extract](../FsCPS/Examples/dell-base-ip.yang) in the `Examples` folder in the source).

The type `IP` has been generated directly from the model, and provides a wrapper to
manipulate CPS objects representing IP addresses. For further details about the mapping
between YANG models and .NET types, see [YANGProvider internals](YANGProviderInternals.md).

As an example, let's say that we want to enumerate all interfaces with all their addresses.

```fsharp
// Get info on all the interfaces
match IP.Ipv4().Get() with
| Error e -> printf "Error: %s\n" e
| Ok res ->
    res |> List.iter (fun x ->
        // Validate each single returned object
        match IP.Ipv4(x) with
        | Error e -> printf "Validation error: %s\n" (e.ToString())
        | Ok iface ->
            // Print interface name and address
            printf "Interface name: %s\n" iface.Name.Value
            for i in 0 .. (iface.Address.Count - 1) do
                printf "Address: %O/%d\n"
                       (System.Net.IPAddress(iface.Address.[i].Ip.Value))
                       iface.Address.[i].PrefixLength.Value
            printf "\n"
    )
```

A lot is happening here. First of all, let's try to understand what members have been generated
by the provider. Each YANG `container` and `list` is mapped to a class with the same name
and a static **factory method** on the root type (in this case, the type `IP`):
the referenced model describes a list named `ipv4`, so this means that it is mapped to a type
`Ipv4` and a factory method with the same name. Each of these factory methods has two
overloads:

- One with no parameters, which creates a new CPS object.
- One which accepts an existing CPS object and performs some validation on it.

So, in the first line of our script we created a new CPS object with the key `base-ip/ipv4` and
performed a `get` request. Note how it was not needed to explicitly pass the key since it
is being extracted automatically extracted from the YANG model. Also, the `Get` method is just
a shortcut for `CPSTransaction.Get` (there are other shortcut methods, one for each operation type:
these methods are useful because most of the times we create transactions with just one object).

If the `get` request completed successfully, we take each single object and pass it to the factory
method `IP.Ipv4`: this overload of the method performs some validation of the given object,
and returns a typed wrapper if it succeeds. We can now access all the data as
properties on the wrapper type: `leaf`s are mapped to properties of primitive types, while
`container`s and `list`s are mapped to properties with more complex types.
For further information about this mapping, see [YANGProvider internals](YANGProviderInternals.md).

Example execution:

```
[...]
Interface name: e101-023-0

Interface name: e101-024-0

Interface name: e101-026-0
Address: 10.1.0.1/16
Address: 10.0.0.1/16
Address: 10.2.0.1/24

Interface name: e101-025-0
[...]
```

As you can see, the type provider created **wrapper types** that allowed **type-safe** access
to the data of a CPS object: this API is much safer and friendlier of the raw untyped API.

> Note: the type provider API is not complete. There are a lot of rough edges, but also
> a lot of ideas to improve it.