namespace ParserCombinator

type CharParser<'state, 'error, 'output> = Parser<char, 'state, 'error, 'output>
