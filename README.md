# elm-bytes-parser [![Build Status](https://travis-ci.com/zwilias/elm-bytes-parser.svg?branch=master)](https://travis-ci.com/zwilias/elm-bytes-parser)
> Parse elm/bytes using composable parsers with errors and context tracking.

`zwilias/elm-bytes-parser` builds on top of `elm/bytes#Bytes.Decode` and adds
functionality I've found useful when dealing with (byte-aligned) binary formats.
Note that most of these features are relevant to creating parsers, and less
relevant to end-users of your decoder/parser.

## Informative errors

When debugging parsers, I've found it very useful to know what went wrong. Where
`Bytes.Decode.decode` returns a `Maybe a`, this package returns a `Result (Error
context error) a`, where the `error` type is parametrized so one can use simple
strings, or a custom type, or whatever you fancy, where the `error` type is
parametrized so one can use simple strings, or a custom type, or whatever you
fancy.

Errors include the offset at which they occurred. When reading goes out of
bounds, the error includes how many bytes you tried to read and the offset at
which you tried to do so. When `fail` is used, it can tell you where exactly
that happened.

## Context for errors

Parsers can run in a given context. When errors occur, they
are (recursively) tagged with the context in which they occured, which means you
get to know both the label of the context, as well as the offset wwhere the
context started.

## Random reads

Quite a few binary formats include fixed-width "information" sections, with
offset to data sections. To make such formats easier to parse, this package
makes it fairly trivial to run a piece of a parser as a "random access" parser,
meaning it runs at specified offset, relative to a fixed position. This fixed
position could be the start of the stream (~> absolute offset) or relative to
some previously parsed marker. See the `randomAccess` docs for more on how to
use this!

Feedback is very welcome. I'd be especially interested in how people end up
using this!
