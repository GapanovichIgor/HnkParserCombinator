module ParserCombinator.Primitives

open ParserCombinator.Composition

let oneCond<'i, 's> (conditionDescription: string) (condition: 'i -> bool) (tape: Tape<'i>, state: 's) : ParseResult<'i, 's> =
    tape.MoveNext()
    let position = tape.Position

    match tape.Current with
    | Some i when condition i ->
        { value = i
          state = state
          position = position
          length = 1 }
        |> Ok
    | _ ->
        tape.MoveBack(1)

        { state = state
          position = position
          expectedDescription = $"item satisfying the condition: %s{conditionDescription}" }
        |> Error

let oneOrMoreCond<'i, 's> (conditionDescription: string) (condition: 'i -> bool) (tape: Tape<'i>, state: 's) : ParseResult<'i array, 's> =
    let position = tape.Position + 1

    let rec loop advanceCount =
        tape.MoveNext()
        let advanceCount = advanceCount + 1

        match tape.Current with
        | Some i when condition i -> loop advanceCount
        | _ ->
            tape.MoveBack(advanceCount)

            if advanceCount > 1 then
                let items = tape.Consume(advanceCount - 1)

                { value = items
                  state = state
                  position = position
                  length = items.Length }
                |> Ok
            else
                { state = state
                  position = position
                  expectedDescription = $"one or more items satisfying the condition: %s{conditionDescription}" }
                |> Error

    loop 0

let zeroOrMoreCond<'i, 's> (condition: 'i -> bool) (tape: Tape<'i>, state: 's) : ParseResult<'i array, 's> =
    let position = tape.Position + 1

    let rec loop advanceCount =
        tape.MoveNext()
        let advanceCount = advanceCount + 1

        match tape.Current with
        | Some i when condition i -> loop advanceCount
        | _ ->
            tape.MoveBack(advanceCount)
            let items = tape.Consume(advanceCount - 1)

            { value = items
              state = state
              position = position
              length = items.Length }
            |> Ok

    loop 0

let skipAny<'i, 's> (count: int) (tape: Tape<'i>, state: 's) : ParseResult<unit, 's> =
    assert (count > 0)

    let position = tape.Position + 1

    let rec loop i =
        if i = count then
            { value = ()
              state = state
              position = position
              length = count }
            |> Ok
        else
            tape.MoveNext()
            let i = i + 1

            if tape.Current.IsSome then
                loop i
            else
                tape.MoveBack(i)

                { state = state
                  position = position
                  expectedDescription = $"any %i{count} items" }
                |> Error

    loop 0

let skipOne<'i, 's when 'i: equality> (item: 'i) (tape: Tape<'i>, state: 's) : ParseResult<unit, 's> =
    tape.MoveNext()

    let position = tape.Position

    match tape.Current with
    | Some i when i = item ->
        { value = ()
          state = state
          position = position
          length = 1 }
        |> Ok
    | _ ->
        tape.MoveBack(1)

        { state = state
          position = position
          expectedDescription = $"item {item}" }
        |> Error

let skipZeroOrMoreCond<'i, 's> (pred: 'i -> bool) (tape: Tape<'i>, state: 's) : ParseResult<unit, 's> =
    let position = tape.Position + 1

    let rec loop advanceCount =
        tape.MoveNext()
        let advanceCount = advanceCount + 1

        match tape.Current with
        | Some i when pred i -> loop advanceCount
        | _ ->
            tape.MoveBack(1)

            { value = ()
              state = state
              position = position
              length = advanceCount - 1 }
            |> Ok

    loop 0

let zeroOrMoreAnyWithTerminator<'i, 's when 'i: equality> (terminator: 'i) : Parser<'i, 's, 'i array> =
    zeroOrMoreCond ((<>) terminator) .>> skipOne terminator
