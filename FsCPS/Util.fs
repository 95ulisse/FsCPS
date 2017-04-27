[<AutoOpen>]
module FsCPS.Util


type Result<'TOk, 'TError> =
| Ok of 'TOk
| Error of 'TError

module internal Result =

    let fromOption opt =
        match opt with
        | Some x -> Ok x
        | None -> Error ()

    let bind f res =
        match res with
        | Ok x -> f x
        | Error x -> Error x

    let bindError f res =
        match res with
        | Ok x -> Ok x
        | Error x -> f x

    let map f res =
        match res with
        | Ok x -> Ok (f x)
        | Error x -> Error x

    let okOrThrow f res =
        match res with
        | Ok x -> x
        | Error e -> f e


let internal (>>=) res f =
    Result.bind f res


let internal (|>>) res f =
    Result.map f res


let internal isNull value =
    match value with
    | null -> true
    | _ -> false


let internal foldResult f initialState xs =
    xs |> Seq.fold (fun state x ->
        match state with
        | Ok s -> f s x
        | Error _ -> state
    ) (Ok(initialState))