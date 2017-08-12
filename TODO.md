# Todo and improvements

> These are just my personal random notes, they will soon disappear from the repository.
> Many of these things do not even make sense, I just write down everything that pops off my head.

- Replace type Result with a type that differentiates between a common error and a fatal one.
  Something like:

  ```
  type Result<'TOk, 'TError> =
  | Ok of 'TOk
  | Error of 'TError
  | Fatal of Exception
  ```

- YANG parser:
  - Add more builtin models.
  - Finish implementing the spec.
  - Refactor to make it more functional instead of OO.
  - Unify post-parse consistency checks like `EnsureRefs`, `CheckRequiredProperties` and so on.
    Maybe just a single method called `CheckConsistency`?
  - Add a check to limit the properties that can be added to a type:
    e.g., an enum cannot have the UnionMembers property.
  - Circular imports.
  - Add active patterns to recognize the base types.
  - Check that data node names are unique in their scope.

- Check for leaks of native memory.
  The `>>=` and `|>>` operators used in the code usually free memory only in case of success.
  Maybe wrap the native pointers in an `IDisposable` that will automatically release the memory.
  To use that, maybe an helper like `Result.using` or an operator `>>?`.

- Native interop:
  - Find a way to wake up a thread suspended on a cps_api_wait_for_event call. (`CPSEventObservable`)
  - Find a way to unregister a server. (`CPSServerHandle`)
    - Being unable to stop these threads is what causes crashes in the mono runtime.
	- Also, maybe this is the cause of the bug that causes the library to bail out with error NO_SERVICE.
	  Running the tests in the module `FsCPS.Tests.Server` more that once does not work.
	  By looking at the output of strace, it looks like that the library binds the server on a unix socket,
	  but then connects to another where no one is listening.
	  
- Add an API to explore paths and IDs, similar to the Python `cps.info`.
  This might lead to the implementation of a type provider to have paths checked at compile time.

- YANGProvider:
  - Store the paths created in a generated type and reference them by id, to avoid object construction every time.
  - Autogenerate validation code based on the YANG model.
  - Support more complex YANG types, like enums or unions.
  - Find a way to make it less dependent on the native library. Find how the native attribute ids are generated
    from the paths and reimplement it in managed code. This should allow testing the provider without the native lib
	and using models not already registered to the CPS.

- Update the examples to reflect the new API.

- Create a Nuget packet.