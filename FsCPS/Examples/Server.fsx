#I "../bin/Debug"
#r "FsCPS.dll"

open FsCPS

type ExampleCPSServer() =
    interface ICPSServer with
        
        member this.Get(req: CPSObject) =
            Ok([ CPSObject("example") ] :> seq<_>)

        member this.Set(req: CPSObject, opType: CPSOperationType) =
            Ok(CPSObject("rollback-object"))

        member this.Rollback(rollback: CPSObject) =
            Ok()

// Registers the CPS server
let handle =
    match CPS.RegisterServer(CPSKey("1.34."), ExampleCPSServer()) with
    | Ok handle -> handle
    | Error e -> invalidOp (sprintf "Server registration failed: %s\n" e)

// Pause
printf "ExampleCPSServer started. Press enter to stop.\n"
System.Console.ReadLine()

// Stops the server
handle.Cancel()