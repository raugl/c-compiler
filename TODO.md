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
[ ] Change mmap to manually coping the input file into memory. Replace tabs, newlines etc. with a standard set of chars.
[ ] Implement preprocessor tokens.
[ ] Implement the preprocessor.
[ ] Write some more tests
[ ] Make use of [this article](https://en.wikipedia.org/wiki/Recursive_descent_parser) to implement the parser.
[ ] Use an arena allocator for all the inter-stage data and maybe also provide a gpa for internal stage use.
