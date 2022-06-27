module HnkParserCombinator.Primitives

open HnkParserCombinator.Composition

type PrimitiveError<'i> =
    | UnexpectedItem of 'i
    | UnexpectedEndOfStream

let oneCond<'i, 's> (cond : 'i -> bool) (tape : Tape<'i>, state : 's) : ParseResult<'i, 's, PrimitiveError<'i>> =
   tape.MoveNext()
   match tape.Current with
   | Some i ->
       if cond i then
           { value = i
             state = state
             length = 1 }
           |> Ok
        else
            tape.MoveBack(1)
            Error(UnexpectedItem i)
   | _ ->
       tape.MoveBack(1)
       Error UnexpectedEndOfStream

let skipAny<'i, 's> (count : int) (tape : Tape<'i>, state : 's) : ParseResult<unit, 's, PrimitiveError<'i>> =
    assert (count > 0)

    let rec loop i =
        if i = count then
            { value = ()
              state = state
              length = count }
            |> Ok
        else
            tape.MoveNext()
            let i = i + 1
            if tape.Current.IsSome then
                loop i
            else
                tape.MoveBack(i)
                Error UnexpectedEndOfStream

    loop 0

let skipOne<'i, 's when 'i : equality> (item : 'i) (tape : Tape<'i>, state : 's) : ParseResult<unit, 's, PrimitiveError<'i>> =
    tape.MoveNext()
    match tape.Current with
    | Some i ->
        if i = item then
            { value = ()
              state = state
              length = 1 }
            |> Ok
        else
            tape.MoveBack(1)
            Error(UnexpectedItem i)
    | None ->
        tape.MoveBack(1)
        Error UnexpectedEndOfStream

let tryMapOne (map : 'i -> 'o option) (tape : Tape<'i>, state : 's) : ParseResult<'o, 's, PrimitiveError<'i>> =
    tape.MoveNext()
    match tape.Current with
    | Some i ->
        match map i with
        | Some result ->
            { value = result
              state = state
              length = 1 }
            |> Ok
        | None ->
            tape.MoveBack(1)
            Error(UnexpectedItem i)
    | None ->
        tape.MoveBack(1)
        Error UnexpectedEndOfStream

let zeroOrMoreCond<'i, 's> (cond : 'i -> bool) (tape : Tape<'i>, state : 's) : ParseResult<'i array, 's, PrimitiveError<'i>> =
    let rec loop advanceCount =
        tape.MoveNext()
        let advanceCount = advanceCount + 1
        match tape.Current with
        | Some i when cond i ->
            loop advanceCount
        | _ ->
            tape.MoveBack(advanceCount)
            let items = tape.Consume(advanceCount - 1)
            { value = items
              state = state
              length = items.Length }
            |> Ok

    loop 0

let skipZeroOrMoreCond<'i, 's> (pred : 'i -> bool) (tape : Tape<'i>, state : 's) : ParseResult<unit, 's, PrimitiveError<'i>> =
    let rec loop advanceCount =
        tape.MoveNext()
        let advanceCount = advanceCount + 1
        match tape.Current with
        | Some i when pred i ->
            loop advanceCount
        | _ ->
            tape.MoveBack(1)
            { value = ()
              state = state
              length = advanceCount - 1 }
            |> Ok

    loop 0

let oneOrMoreCond<'i, 's> (cond : 'i -> bool) (tape : Tape<'i>, state : 's) : ParseResult<'i array, 's, PrimitiveError<'i>> =
    let rec loop advanceCount =
        tape.MoveNext()
        let advanceCount = advanceCount + 1

        let stop e =
            tape.MoveBack(advanceCount)
            if advanceCount > 1 then
                let items = tape.Consume(advanceCount - 1)
                { value = items
                  state = state
                  length = items.Length }
                |> Ok
            else
                Error e

        match tape.Current with
        | Some i ->
            if cond i then
                loop advanceCount
            else
                stop (UnexpectedItem i)
        | None ->
            stop UnexpectedEndOfStream

    loop 0

let zeroOrMoreAnyWithTerminator<'i, 's when 'i : equality> (terminator : 'i) : Parser<'i, 's, PrimitiveError<'i>, 'i array> =
    zeroOrMoreCond ((<>) terminator)
    .>>
    skipOne terminator
