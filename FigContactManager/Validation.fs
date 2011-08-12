module FigContactManager.Validation

let bind f =
    function
    | Choice2Of2 x -> Choice2Of2 x
    | Choice1Of2 x -> f x

let map f =
    function
    | Choice2Of2 x -> Choice2Of2 x
    | Choice1Of2 x -> Choice1Of2 (f x)

let ap f x =
    match f,x with
    | Choice1Of2 f, Choice1Of2 x -> Choice1Of2 (f x)
    | Choice2Of2 e, _ -> Choice2Of2 e
    | _, Choice2Of2 e -> Choice2Of2 e

let apv f x =
    match f,x with
    | Choice1Of2 f, Choice1Of2 x -> Choice1Of2 (f x)
    | Choice2Of2 e1, Choice2Of2 e2 -> Choice2Of2 (e1 @ e2)
    | Choice2Of2 e, _ -> Choice2Of2 e
    | _, Choice2Of2 e -> Choice2Of2 e

let getOrFail =
    function
    | Choice1Of2 x -> x
    | Choice2Of2 e -> failwithf "Validation errors: %A" e