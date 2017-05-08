# Todo and improvements

- Replace type Result with a type that differentiates between a common error and a fatal one.
  Something like:

  ```
  type Result<'TOk, 'TError> =
  | Ok of 'TOk
  | Error of 'TError
  | Fatal of Exception
  ```

- Check for leaks of native memory.
  The `>>=` and `|>>` operators used in the code usually free memory only in case of success.
  Maybe wrap the native pointers in an `IDisposable` that will automatically release the memory.
  To use that, maybe an helper like `Result.using` or an operator `>>?`.

- Embedded attributes.
  At the moment, the library supports just one "level" (?) of attributes.
  I could not find any documentation about this.
  Also fix functions like `AttrIdFromPath` that have magic offsets inside.

- YANG parser:
  - Add more builtin models.
  - Finish implementing the spec.
  - Rewrite the type properties to be something more type-safe.

- Native interop:
  - Find a way to wake up a thread suspended on a cps_api_wait_for_event call. (`CPSEventObservable`)
  - Find a way to unregister a server. (`CPSServerHandle`)
    - Being unable to stop these threads is what causes crashes in the mono runtime.
	- Also, maybe this is the cause of the bug that causes the library to bail out with error NO_SERVICE.
	  Running the tests in the module `FsCPS.Tests.Server` more that once does not work.
	  By looking at the output of strace, it looks like that the library binds the server on a unix socket,
	  but then connects to another where no one is listening.