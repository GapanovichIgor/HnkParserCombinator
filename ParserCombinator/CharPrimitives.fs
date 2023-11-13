module ParserCombinator.CharPrimitives

open Composition
open ParserCombinator.Primitives

let constString<'state> (str: string) : Parser<char, 'state, PrimitiveError<char>, unit> =
    if str.Length = 1 then
        skipOne str[0]
    else
        str |> Seq.map skipOne |> Seq.reduce (>>.)
