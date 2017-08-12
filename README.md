# FsCPS

F# bindings and type providers for the Control Plane Services API.

FsCPS exposes all the functionalities of the Control Plane Services to the managed world:
it allows executing operations on objects, writing servers using any .NET language and
sending and listening for events. Also, a type provider that uses the information in the YANG models
makes writing code much more easy and type-safe.

### Control Plane... What?

Long story short, the Control Plane Services (CPS, from now on) are an API for system-wide communication
implemented in some OSes for switches. Initially developed by Dell for its own [Dell OS 10][1], the CPS
has been open sourced and integrated in other projects, namely [SONiC][2] and [OpenSwitch][3].

You can imagine the CPS as a single shared message bus: applications can **send messages**
or **register the interest** in receiving them. Describing the CPS as a simple *publish/subscribe*
infrastructure, though, is very simplistic: its main purpose is providing a common programmatic API
for applications to configure system components and hardware. Applications can request the execution
of operations (namely `get`, `set`, `create` and `delete`) on a particular **object**, like a
database. Applications requesting the operations are called **clients**, while those who actually
execute the requests of the clients are called **servers**.
In the CPS infrastructure, everything is modeled as an object: a fan, a network interface,
an IP address, a VLAN, and a lot more. An object is made up of two parts: a **key**,
which basically tells what *kind* of object it is, and a set of **attributes**, each with
a name and a value.

The CPS does not enforce any rule about the contents of those objects
(aside from the structure key + attributes), so how do we know what to put in an object?
A set of formal models written in a language called [YANG][4] has been in fact provided
to specify which combinations of keys and attributes are valid. As an example,
[this][5] is the set of models provided by OpenSwitch.

This should be enough to understand the basic concepts of the CPS, but obviously we skipped over the
details and the underlying complexity of the infrastructure. You can read more about the inner workings
[here][6].

[1]: http://www.dell.com/en-us/work/shop/povw/open-platform-software
[2]: http://azure.github.io/SONiC/
[3]: https://www.openswitch.net/
[4]: https://tools.ietf.org/html/rfc6020
[5]: https://github.com/open-switch/opx-base-model/tree/master/yang-models
[6]: https://github.com/open-switch/opx-docs/wiki/Application-programming



## Getting started

Click [here](doc/GettingStarted.md) to see some example usages of the library.



## Installing

To install FsCPS, just install the corresponding NuGet package from the Package Manager console:

```
PM> Install-Package FsCPS
```



## Building from source

Clone the repo, then open the solution `FsCPS.sln` in Visual Studio and hit Compile.



## Running the tests

The tests are written using [FsCheck][7] and [xUnit][8].
Open the solution in Visual Studio and run the tests using its Test Explorer.

> Note: Expect some tests to fail. Some of them are to be run directly on a system
> where the CPS is available. After setting up CI, the process of running tests
> and building from the command line will be streamlined.

[7]: https://fscheck.github.io/FsCheck/
[8]: https://xunit.github.io/



## Known bugs and limitations

- The YANG parser is nowhere near complete.
- The YANGProvider does not support all leaf types: enums and unions are not mapped yet.
- An application that starts listening for events or registers a server cannot be terminated gracefully.



## Versioning

FsCPS uses [SemVer](http://semver.org/) for versioning. For the versions available,
see the [tags on this repository](https://github.com/95ulisse/FsCPS/tags).

Also note that the API is not final, and breaking changes might happen, even though they will be avoided when possible.
Full stabilization of the API will be marked with a 1.0 release.



## License

This project is licensed under the MIT License. See the [LICENSE.md](LICENSE.md) file for details.