namespace ParserCombinator

type Parser<'input, 'state, 'error, 'output> = Tape<'input> * 'state -> ParseResult<'output, 'state, 'error>
