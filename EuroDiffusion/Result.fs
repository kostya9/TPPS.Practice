module EuroDiffusion.Result

type Result<'T> =
    | Error of string
    | Success of 'T
    
let isError = function
    | Error(_) -> true
    | _ -> false
    
let chooseSuccess = function
    | Success(c) -> Some c
    | _ -> None
    
let isSomeOrError c =
    match c with
    | Success(Some(c)) -> true
    | Error(e) -> true
    | Success(None) -> false

let chooseSomeOrError c =
    match c with
    | Success(Some(c)) -> Some(Success(c))
    | Error(e) -> Some(Error(e))
    | Success(None) -> None
    
let bind f1 result =
    match result with
    | Success(value) -> f1 value
    | Error(errorMsg) -> Error(errorMsg)