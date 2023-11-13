module ParserCombinator.Composition

let inline private combine valueSelector (parser1: Parser<'input, 'state, 'outputA>) (parser2: Parser<'input, 'state, 'outputB>) =
    fun (tape: Tape<_>, state) ->
        match parser1 (tape, state) with
        | Ok success1 ->
            match parser2 (tape, success1.state) with
            | Ok success2 ->
                { value = valueSelector (success1.value, success2.value)
                  state = success2.state
                  length = success1.length + success2.length }
                |> Ok
            | Error error2 ->
                tape.MoveBack(success1.length)
                Error error2
        | Error error1 -> Error error1

let (>>.) p1 p2 = combine snd p1 p2

let (.>>) p1 p2 = combine fst p1 p2

let (.>>.) p1 p2 = combine id p1 p2

let chooseFirstLongest (parsers: Parser<'input, 'state, 'output> list) : Parser<'input, 'state, 'output> =
    assert (parsers.Length >= 2)

    let combine result1 result2 =
        match result1, result2 with
        | Ok success1, Ok success2 ->
            if success1.length < success2.length then
                Ok success2
            else
                Ok success1
        | Error _, Ok success2 -> Ok success2
        | Ok success1, Error _ -> Ok success1
        | Error error1, Error _ -> Error error1

    fun (tape, state) ->
        let result =
            parsers
            |> Seq.map (fun p ->
                let result = p (tape, state)

                match result with
                | Ok success -> tape.MoveBack(success.length)
                | _ -> ()

                result)
            |> Seq.reduce combine

        match result with
        | Ok success -> tape.MoveForward(success.length)
        | _ -> ()

        result

let optional (parser: Parser<'input, 'state, 'output>) : Parser<'input, 'state, 'output option> =
    fun (tape, state) ->
        match parser (tape, state) with
        | Ok success ->
            { value = Some success.value
              state = success.state
              length = success.length }
            |> Ok
        | Error _ ->
            { value = None
              state = state
              length = 0 }
            |> Ok

let zeroOrMore (parser: Parser<'input, 'state, 'output>) : Parser<'input, 'state, 'output list> =
    fun (tape, state) ->
        let mutable keepGoing = true
        let mutable state = state

        let results =
            [ while keepGoing do
                  match parser (tape, state) with
                  | Ok o ->
                      yield o
                      state <- o.state
                  | Error _ -> keepGoing <- false ]

        { value = results |> List.map (fun s -> s.value)
          state = state
          length = results |> List.sumBy (fun s -> s.length) }
        |> Ok

let zeroOrMoreDelimited (delimiter: Parser<'input, 'state, _>) (parser: Parser<'input, 'state, 'output>) : Parser<'input, 'state, 'output list> =
    fun (tape, state) ->
        let mutable keepGoing = true
        let mutable moreRequired = false
        let mutable error = None
        let mutable state = state

        let results =
            [ while keepGoing do
                  match parser (tape, state) with
                  | Ok success ->
                      yield success
                      state <- success.state

                      match delimiter (tape, state) with
                      | Ok success ->
                          moreRequired <- true
                          state <- success.state
                      | _ -> keepGoing <- false
                  | Error e ->
                      if moreRequired then
                          error <- Some e ]

        match error with
        | Some e -> Error e
        | None ->
            { value = results |> List.map (fun s -> s.value)
              state = state
              length = results |> List.sumBy (fun s -> s.length) }
            |> Ok

let oneOrMore (parser: Parser<'input, 'state, 'output>) : Parser<'input, 'state, 'output list> =
    fun (tape, state) ->
        let mutable keepGoing = true
        let mutable error = None
        let mutable state = state

        let results =
            [ while keepGoing do
                  match parser (tape, state) with
                  | Ok o ->
                      yield o
                      state <- state
                  | Error e ->
                      error <- Some e
                      keepGoing <- false ]

        if results.Length > 0 then
            { value = results |> List.map (fun s -> s.value)
              state = state
              length = results |> List.sumBy (fun s -> s.length) }
            |> Ok
        else
            Error(Option.get error)

let orElse (fallbackParser: Parser<'input, 'state, 'output>) (mainParser: Parser<'input, 'state, 'output>) : Parser<'input, 'state, 'output> =
    fun (tape, state) ->
        match mainParser (tape, state) with
        | Error _ -> fallbackParser (tape, state)
        | Ok success -> Ok success

let commitOnSuccess (parser: Parser<'input, 'state, 'output>) : Parser<'input, 'state, 'output> =
    fun (tape, state) ->
        let result = parser (tape, state)

        match result with
        | Ok success -> tape.Commit(success.length)
        | _ -> ()

        result
