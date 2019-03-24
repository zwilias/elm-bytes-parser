# elm-bytes-parser [![Build Status](https://travis-ci.com/zwilias/elm-bytes-parser.svg?branch=master)](https://travis-ci.com/zwilias/elm-bytes-parser)
> Parse elm/bytes using composable parsers with errors and context tracking.

`zwilias/elm-bytes-parser` builds on top of `elm/bytes#Bytes.Decode` and adds
functionality I've found useful when dealing with (byte-aligned) binary formats.
Note that most of these features are relevant to creating parsers, and less
relevant to end-users of your decoder/parser.

When debugging parsers, I've found it very useful to know what went wrong. Where
`Bytes.Decode.decode` returns a `Maybe a`, this package returns a `Result (Error
context error) a`.

Errors include the offset at which they occurred. When reading goes out of
bounds, the error includes how many bytes you tried to read and the offset at
which you tried to do so. When `fail` is used, it can tell you where exactly
that happened.

Note that you can use custom error types and aren't confined to a simple
`String`, so error reporting can generate highly informative errors.

Last but not least, parsers can run in a given context. When errors occur, they
are (recursively) tagged with the context in which they occured, which means you
get to know both the label of the context, as well as the offset wwhere the
context started.

Feedback is very welcome. I'd be especially interested in how people end up
using this!
