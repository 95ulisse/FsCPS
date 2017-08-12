# YANGProvider internals

This document describes the internals of the type provider `YANGProvider`, in particular,
we will focus on the mapping between YANG nodes and .NET types.

> As a reference, you can use the [dell-base-ip.yang](../FsCPS/Examples/dell-base-ip.yang)
> in the `Examples` folder, since sometimes this model will be used as an example.



## Mapping between YANG models and .NET types

First of all, a little recap of some key principles of the provider:

- It's an **erased** type provider.
- Leafs and leaf-lists are mapped to properties.
- Containers and lists are mapped to properties and new types.

If we look at the IP model mentioned above, we can see that it models a structure
similar to this (we cut off IPv6 because the structure is very similar):

```
── ipv4 [list]
   ├── ifindex: uint32
   ├── enabled: boolean
   ├── forwarding: boolean
   ├── vrf-id: uint32
   ├── name: string
   └── address [list]
       ├── ip: binary
       └── prefix-length: uint8
```

The type provider will have to recreate a similar structure within the .NET type system.
This tree is traversed depth-first and every data node is mapped to a member.
Let's see what happens when we write this:

```fsharp
type IP = YANGProvider<fileName = "/somewhere/dell-base-ip.yang">
```

- First of all, the root type `IP` is created. This type does not represent anything
  in the YANG model, but will be the parent of all the nested types that will be created.
  Also, a stack of types is created, and `IP` gets pushed onto it.

- The `ipv4` node is visited: a new type `Ipv4` is created as a nested type of `IP`
  and pushed to the stack. Two factory methods with the same name are added
  to the root type (`IP`):
  - One with no parameters, which creates a new `Ipv4` instance.
  - One which accepts an existing CPS object, performs some validation on it and converts
    it to an instance of `Ipv4`.

  **Note**: these factory methods are the only way to construct instances of the generated types.

- The node `ifindex` is visited. Since this is a leaf, it is mapped to a property of the type
  on the top of the stack, which happens to be `Ipv4`. The type of the property is mapped
  to the closest .NET type available.

- The nodes `enabled`, `forwarding`, `vrf-id` and `name` are all leafs, so they will all
  generate a new property on the type `Ipv4`.

- The node `address` is visited: a new type `Address` is created as a nested type of the
  topmost type on the stack (which is `Ipv4`). The two factory methods are added to the
  root type, and a new property with the same name is added to the topmost type on the stack
  (always `Ipv4`). The type `Address` is pushed to the stack.

- The leafs `ip` and `prefix-length` are visited and mapped to properties of the topmost type,
  which now is `Address`.

- We go up one level since there are no more children to visit, and pop `Address`
  from the stack.

- We go up again, and pop `Ipv4`.

- There are no more children to visit, so on the stack there's only the final root type `IP`
  fully populated. This root type will be the final generated type.

The final .NET type hierarchy is:

```
── IP [class]
   ├── Ipv4() [static method]
   ├── Ipv4(CPSObject) [static method]
   ├── Address() [static method]
   ├── Address(CPSObject) [static method]
   └── Ipv4 [class]
       ├── Ifindex: uint32
       ├── Enabled: bool
       ├── Forwarding: bool
       ├── VrfId: uint32
       ├── Name: string
       ├── Address: AddressCollection
       └── Address [class]
           ├── Ip: byte[]
           └── PrefixLength: uint8
```

### Data nodes

There are 4 YANG data nodes supported by the provider:

- **Leaf nodes**: leafs are mapped to plain .NET properties. The type of the property
  is the .NET type closest to the original YANG type, wrapped in an *option*. This means
  that a leaf node like this:
  
  ```
  leaf my-node {
      type uint32;
  }
  ```

  Is mapped to a property `MyNode` of type `uint32 option`, which is both **readable** and
  **writeable**. The presence of an `option` is needed because it is a signal of the absence
  of the value in the object: reading a `None` means that the value is not present in the object,
  and writing a `None` is equivalent to removing the attribute.

- **Leaf-list nodes**: leaf-lists are exactly identical to simple leafs, except that their
  .NET type is `'a list option`.

- **Container nodes**: containers group other nodes together, so this means that we have
  to generate a new type. For each container node a new .NET type with the same name
  is generated, and a **read-only** property is added to the topmost type on the stack.
  As an example, this:

  ```
  container my-container {
      leaf my-leaf {
          type int32;
      }
  }
  ```

  Generates a new type called `MyContainer` (with a single property `MyLeaf` of type `int32 option`)
  and a new property `MyContainer` of type `MyContainer` in the parent type.

  For each container node, **two factory methods** (which act as constructors) are added
  to the root type:

  - One with no parameters, which creates a new instance of the generated container type.
  - One which accepts an existing CPS object, performs some validation on it and converts
    it to an instance of the generated container type.

- **List nodes**: list nodes can be thought as lists of containers, so this means that we still
  generate a new .NET type and a new property like in the case of a simple container, but
  the type of the generated property is different:

  ```
  list my-list {
      leaf my-leaf {
          type: int32;
      }
  }
  ```

  In this example, we generate a new type called `MyList` (with a single property
  `MyLeaf` of type `int32 option`) and a new property `MyList` of type `MyListCollection`
  in the parent type. The type of the property is a **fake collection-like type** which allows
  access using an **indexer**. The generated property and the indexer of the fake collection-like
  type are **read-only**.

### Leaf types

YANG types are mapped to .NET types according to the following table:

| YANG type     |    | .NET Type           | Notes                |
|---------------|----|---------------------|----------------------|
| `empty`       | -> | \<not implemented\> | Not yet implemented. |
| `boolean`     | -> | `bool`              |                      |
| `int8`        | -> | `int8`              |                      |
| `int16`       | -> | `int16`             |                      |
| `int32`       | -> | `int32`             |                      |
| `int64`       | -> | `int64`             |                      |
| `uint8`       | -> | `uint8`             |                      |
| `uint16`      | -> | `uint16`            |                      |
| `uint32`      | -> | `uint32`            |                      |
| `uint64`      | -> | `uint64`            |                      |
| `string`      | -> | `string`            |                      |
| `binary`      | -> | `byte`              |                      |
| `decimal64`   | -> | `double`            |                      |
| `enumeration` | -> | \<not implemented\> | Not yet implemented. |
| `union`       | -> | \<not implemented\> | Not yet implemented. |



## Erased type

As we said earlier, the `YANGProvider` is an **erased** type provider. What do the provided types
erase to? Inside the module `FsCPS.TypeProviders.Runtime.YANGProviderRuntime` there are
functions and types that are meant to be used only by the code generated by the provider,
and among them, there's `PathBuilder`, the base type of all the erased types
([source code](../FsCPS/TypeProviders/YANGProviderRuntime.fs)).

As the name implies, a `PathBuilder` provides a simple way to build a *path* inside a CPS
object which can traverse nested containers and lists. Let's see an example of how to use it
(Remember this type, is not meant to be used directly from user code):

```fsharp
PathBuilder.FromObject(anObj)
    .Access(1000UL)
    .Access(1001UL)
    .Indexer(2)
    .Access(42UL)
    .ReadLeaf<int>()
```

This means the following:

```
anObj
    -> Access container with ID 1000
    -> Access the third element of the list with ID 1001
    -> Read leaf with ID 42
```

But wait, we never said what nodes were containers or lists, how did it figured that out?
The implementation of `PathBuilder` relies on a fundamental assumption:
**You can only read and write `leaf`s and `leaf-list`s. `Container`s and `list`s are just
ways to structure the data.** From this follows immediately that we can't write entire lists
or containers, so we can simplify our path implementation. Let's see a complete example:

```fsharp
type Example = YANGProvider<model = """
    module path-example {
        prefix path;
        namespace "http://example.com";

        // ID: 1
        container root-container {

            // ID: 2
            leaf my-leaf {
                type int32;
            }

            // ID: 3
            container nested-container {
                // ID:4
                leaf nested-leaf {
                    type int32;
                }
            }

            // ID: 5
            list nested-list {
                key "nested-leaf";

                // ID: 6
                leaf nested-leaf {
                    type int32;
                }

                // ID: 7
                container list-container {
                    // ID: 8
                    leaf nested-nested-leaf {
                        type int32;
                    }
                }
            }
        }
    }
""">

// `obj` is erased to a PathBuilder
let obj = Example.RootContainer()

// A simple leaf access (which is the common case):
// obj.Access(2UL).WriteLeaf<int>(Some 42)
obj.MyLeaf <- Some 42

// A leaf inside a container:
// obj.Access(3UL).Access(4UL).WriteLeaf<int>(Some 42)
obj.NestedContainer.NestedLeaf <- Some 42

// A leaf inside a list:
// obj.Access(5UL).Indexer(10).Access(6UL).WriteLeaf<int>(Some 42)
obj.NestedList.[10].NestedLeaf <- Some 42

// You got the idea.
// obj.Access(5UL).Indexer(10).Access(7UL).Access(8UL).WriteLeaf<int>(Some 42)
obj.NestedList.[10].ListContainer.NestedNestedLeaf <- Some 42
```

As you can see from the above examples, any property access is translated into a call
to `Access` and any indexer access ia translated into a call to `Indexer`.
The rules behind this construction are simple:

- A final `Access` means that we are accessing a leaf.
- An `Access` followed by an `Indexer` is an access to a specific element of a list.
- A non-final `Access` is an access to a container.

A final note on the implementation:

```fsharp
type internal AttributePathSegment =
    | Access of CPSAttributeID
    | Indexer

type PathBuilder =
    private {
        Object: CPSObject;
        Segments: AttributePathSegment list;
        Indices: int list
    }
    with
        member this.Access aid =
                { this with Segments = Access (CPSAttributeID aid) :: this.Segments }

        member this.Indexer i =
            { this with Segments = Indexer :: this.Segments;
                        Indices = i :: this.Indices }
```

A `PathBuilder` is fundamentally a triple consisting of a reference to a `CPSObject`, a list
of `AttributePathSegment` and a list of integers. As you can see, its implementation is quite
straightforward, but I'd like to point out why indices are kept in another list.
We could have had a union case `Indexer of int` and it would have worked the same.
And yes, that's right, it would have worked the same, but this implementation leaves room
for a future improvement: instead of recreating the same paths again and again (imagine
in a loop), longer paths can be cached somewhere and then referenced by ID. Caching the
`Access`es causes no problems, but an `Indexer` has an index which is not fixed at build
time, so it needs to be tracked separately, since its value is only known at runtime.

In theory, it should work like this:

```fsharp
type Example = YANGProvider<model = "... the same model above ...">

let obj = Example.RootContainer()
for i in 0 .. 10 do
    // Ideally, we would like to avoid recreating over and over the same
    // paths, so we would like to be able to shortcut this to:
    // PathBuilder.WriteLeaf(obj, 10UL, [ i ], Some 42)
    // Where 10UL is the ID assigned to this specific path.
    obj.NestedList.[i].ListContainer.NestedNestedLeaf <- Some 42
```

**Note: This has not been implemented yet, it's just an idea to justify the implementation.**