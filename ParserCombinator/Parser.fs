namespace ParserCombinator

type Parser<'input, 'state, 'output> = Tape<'input> * 'state -> ParseResult<'output, 'state>
