# To do:
[x] Add source location to tokens
[x] Added error reporting to the lexer
[x] Improve the literal int token.
[x] Improve the literal float token.
[x] Investigate if a top-down descent approach would be better for the lexer.
[x] Allocate literal strings and parse control sequences out of them.
[x] `consumeWhitespace()` gets stuck on newline escapes.
[x] Fix bug where identifiers can't start with keywords.
[x] Implement complete char literal lexing.
[x] Fix unicode rendering issues for log messages.
[x] Change mmap to manually coping the input file into memory. Replace tabs, newlines etc. with a standard set of chars.
[x] Use an arena allocator for all the inter-stage data.
[x] Add local buffer for logging allocations.
[x] Double check the operator and keyword lexers using cppreference.
[x] Implement preprocessor tokens.
[ ] Implement the preprocessor.
[ ] Add basic cli.
[ ] Write some more tests
[ ] Make use of [this article](https://en.wikipedia.org/wiki/Recursive_descent_parser) to implement the parser.
