module HnkParserCombinator.IndentationBasedLanguageKit

open HnkParserCombinator.Primitives
open HnkParserCombinator.Composition

type State =
    { indentationLevels: int list }
    member this.IndentationLevels =
        this.indentationLevels

    static member UpdateIndentationLevels(state, indentaionLevels) =
        { state with indentationLevels = indentaionLevels }

    static member initial = { indentationLevels = [] }

let inline private getIndentationLevels state =
    (^s: (member IndentationLevels: int list) state)

let inline private updateIndentationLevels state indentationLevels =
    (^s: (static member UpdateIndentationLevels: ^s * int list -> ^s) (state, indentationLevels))

let inline parseIndentation
    (blockOpen: 't)
    (newLineDelimiter: 't)
    (blockClose: 't)
    (isWhiteSpace: char -> bool)
    : CharParser< ^s, PrimitiveError<char>, 't list > =

    fun (tape: Tape<char>, state: 's) ->
        let p = zeroOrMoreCond isWhiteSpace

        match p (tape, state) with
        | Ok s ->
            let chars = s.value

            let newLevel = chars |> Array.length

            let rec resolveLevelsAndTokens levStack tokens =
                match levStack with
                | [] -> ([ newLevel ], tokens)
                | topLevel :: ls ->
                    if newLevel > topLevel then
                        (newLevel :: levStack, blockOpen :: tokens |> List.rev)
                    elif newLevel = topLevel then
                        (levStack, newLineDelimiter :: tokens |> List.rev)
                    else
                        resolveLevelsAndTokens ls (blockClose :: tokens)

            let indentationLevels =
                getIndentationLevels state

            let resultLevels, tokens =
                resolveLevelsAndTokens indentationLevels []

            { value = tokens
              state = updateIndentationLevels state resultLevels
              length = s.length }
            |> Ok

        | Error e -> Error e

let inline closeDanglingBlocks (blockClose: 't) (state: ^s) : 't list * 's =
    let rec closeBlocks levels tokens =
        match levels with
        | [ _ ] -> tokens
        | _ :: levelsRest -> closeBlocks levelsRest (blockClose :: tokens)
        | _ -> tokens

    let indentationLevels =
        getIndentationLevels state

    let tokensWithClosedBlocks =
        closeBlocks indentationLevels []

    let state = updateIndentationLevels state []

    (tokensWithClosedBlocks, state)

type SimpleParseDocumentSetup<'t> =
    { parseToken : CharParser<State, PrimitiveError<char>, 't>
      blockOpenToken : 't
      blockCloseToken : 't
      newLineDelimiter : 't
      isWhiteSpace : char -> bool }

let simpleParseDocument (setup: SimpleParseDocumentSetup<'t>) (tape: Tape<char>) =
    let maybeSkipWhitespace =
        skipZeroOrMoreCond setup.isWhiteSpace
        |> commitOnSuccess

    let skipNewLine =
        chooseFirstLongest [ skipOne '\n'
                             skipOne '\r'
                             skipOne '\r' >>. skipOne '\n' ]
        |> commitOnSuccess

    let parseToken =
        setup.parseToken
        |> commitOnSuccess

    let parseLineWithTokens =
        let indentation = parseIndentation setup.blockOpenToken setup.newLineDelimiter setup.blockCloseToken setup.isWhiteSpace
        let lineTokens = oneOrMore (parseToken .>> maybeSkipWhitespace)

        indentation .>>. lineTokens
        >> ParseResult.mapValue (fun (l, r) -> l @ r)

    let parseEmptyLine = maybeSkipWhitespace >> ParseResult.constValue []

    let parseLine =
        parseLineWithTokens |> orElse parseEmptyLine

    let terminateBlocks result =
        match result with
        | Ok s ->
            let tokens, state =
                closeDanglingBlocks setup.blockCloseToken s.state

            { value = s.value @ [ tokens ]
              state = state
              length = s.length }
            |> Ok
        | Error e -> Error e

    let parseDocument =
        let concatLines =
            ParseResult.mapValue List.concat

        zeroOrMoreDelimited skipNewLine parseLine
        >> terminateBlocks
        >> concatLines

    parseDocument (tape, State.initial)