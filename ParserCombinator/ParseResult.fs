namespace ParserCombinator

[<Struct>]
type ParseSuccess<'value, 'state> =
    { value: 'value
      state: 'state
      length: int }

type ParseError<'state> =
    { state: 'state
      expectedDescription: string }

type ParseResult<'value, 'state> = Result<ParseSuccess<'value, 'state>, ParseError<'state>>

module ParseResult =
    let mapValue (f: 'a -> 'b) (result: ParseResult<'a, _>) : ParseResult<'b, _> =
        match result with
        | Ok success ->
            { value = f success.value
              state = success.state
              length = success.length }
            |> Ok
        | Error error -> Error error

    let constValue (value: 'b) (result: ParseResult<'a, _>) : ParseResult<'b, _> =
        match result with
        | Ok success ->
            { value = value
              state = success.state
              length = success.length }
            |> Ok
        | Error e -> Error e

    let mapErrorDescription (f: string -> string) (result: ParseResult<_, _>) : ParseResult<_, _> =
        match result with
        | Ok success -> Ok success
        | Error error ->
            { state = error.state
              expectedDescription = f error.expectedDescription }
            |> Error