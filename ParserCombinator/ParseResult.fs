namespace ParserCombinator

[<Struct>]
type ParseSuccess<'value, 'state> =
    { value: 'value
      state: 'state
      position: int
      length: int }

type ParseError<'state> =
    { state: 'state
      position: int
      expectedDescription: string }

type ParseResult<'value, 'state> = Result<ParseSuccess<'value, 'state>, ParseError<'state>>

module ParseResult =
    let mapValue (f: 'a -> 'b) (result: ParseResult<'a, _>) : ParseResult<'b, _> =
        match result with
        | Ok success ->
            { value = f success.value
              state = success.state
              position = success.position
              length = success.length }
            |> Ok
        | Error error -> Error error

    let mapSuccess (f: ParseSuccess<'a, 's> -> 'b) (result: ParseResult<'a, 's>): ParseResult<'b, 's> =
        match result with
        | Ok success ->
            { value = f success
              state = success.state
              position = success.position
              length = success.length }
            |> Ok
        | Error error -> Error error

    let constValue (value: 'b) (result: ParseResult<'a, _>) : ParseResult<'b, _> =
        match result with
        | Ok success ->
            { value = value
              state = success.state
              position = success.position
              length = success.length }
            |> Ok
        | Error e -> Error e

    let mapErrorDescription (f: string -> string) (result: ParseResult<_, _>) : ParseResult<_, _> =
        match result with
        | Ok success -> Ok success
        | Error error ->
            { state = error.state
              position = error.position
              expectedDescription = f error.expectedDescription }
            |> Error
