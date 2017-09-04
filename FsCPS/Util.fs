namespace FsCPS

[<RequireQualifiedAccess>]
module internal Result =

    let isOk = function
        | Ok _ -> true
        | Error _ -> false

    let fromOption opt =
        match opt with
        | Some x -> Ok x
        | None -> Error ()

    let toOption res =
        match res with
        | Ok x -> Some x
        | Error _ -> None

    let bindError f res =
        match res with
        | Ok x -> Ok x
        | Error x -> f x

    let okOrThrow f res =
        match res with
        | Ok x -> x
        | Error e -> f e

    let pipe other res =
        match res with
        | Ok x ->
            match other with
            | Ok y -> Ok(x, y)
            | Error e2 -> Error e2
        | Error e1 -> Error e1

    let tee f res =
        match res with
        | Ok x ->
            match f x with
            | Ok _ -> res
            | Error e -> Error e
        | Error x -> res

    let foldSequence f initialState xs =
        xs |> Seq.fold (fun state x ->
            match state with
            | Ok s -> f s x
            | Error _ -> state
        ) (Ok(initialState))


[<AutoOpen>]
module internal Operators =

    let internal (>>=) res f =
        Result.bind f res


    let internal (|>>) res f =
        Result.map f res