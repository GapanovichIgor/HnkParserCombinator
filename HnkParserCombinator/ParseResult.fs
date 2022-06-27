namespace HnkParserCombinator

[<Struct>]
type ParseSuccess<'value, 'state> =
    { value: 'value
      state: 'state
      length: int }

type ParseResult<'value, 'state, 'error> = Result<ParseSuccess<'value, 'state>, 'error>

module ParseResult =
    let mapValue (f: 'a -> 'b) (r: ParseResult<'a, _, _>): ParseResult<'b, _, _> =
        match r with
        | Ok ok ->
            { value = f ok.value
              state = ok.state
              length = ok.length }
            |> Ok
        | Error e -> Error e