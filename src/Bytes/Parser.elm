module Bytes.Parser exposing
    ( Parser, run, Error(..)
    , succeed, fail, inContext
    , unsignedInt8, unsignedInt16, unsignedInt32, signedInt8, signedInt16, signedInt32
    , float32, float64
    , string
    , bytes
    , map, map2, map3, map4, map5
    , keep, ignore, skip
    , andThen, oneOf, repeat, Step(..), loop
    , Position, position, startOfInput, randomAccess
    )

{-| Parse `Bytes` with custom error reporting and context tracking.


# Running parsers

@docs Parser, run, Error


# Static parsers

@docs succeed, fail, inContext


# Basic parsers


## Integers

@docs unsignedInt8, unsignedInt16, unsignedInt32, signedInt8, signedInt16, signedInt32


## Floats

@docs float32, float64


## Strings

@docs string


## Bytes

@docs bytes


# Transforming values

@docs map, map2, map3, map4, map5


# Combininig parsers

@docs keep, ignore, skip


# Fancy parsers

@docs andThen, oneOf, repeat, Step, loop


# Random access

@docs Position, position, startOfInput, randomAccess

-}

import Bytes exposing (Bytes)
import Bytes.Decode as Decode exposing (Decoder)


{-| A concrete position in the input.
-}
type Position
    = Position Int


{-| A parser which tracks a certain type of context, a certain type of error and
produces a certain type of value.
-}
type Parser context error value
    = Parser (State -> ParseResult context error value)


type ParseResult context error value
    = Good value State
    | Bad (Error context error)


{-| Describes errors that arise while parsing.

Custom errors happen through [`fail`](#fail), context tracking happens through
[`inContext`](#inContext).

-}
type Error context error
    = InContext context (Error context error)
    | EndOfInput
    | Custom error
    | BadOneOf (List (Error context error))


type alias State =
    { offset : Int
    , stack : List Int
    , input : Bytes
    }


{-| Always succeed with the given value.

    import Bytes.Encode as E
    import Bytes.Parser as P


    E.encode (E.sequence [])
        |> P.run (P.succeed "hi there")
    --> Ok "hi there"

-}
succeed : value -> Parser context error value
succeed val =
    Parser (Good val)


{-| Produce the current offset in the input.

    import Bytes.Encode as E
    import Bytes.Parser as P


    E.encode (E.string "hello")
        |> P.run P.position
    --> Ok P.startOfInput


    parser : P.Parser c e P.Position
    parser =
        P.succeed identity
            |> P.skip 2
            |> P.keep P.position


    E.encode (E.string "hello")
        |> P.run parser
        |> Result.map ((==) P.startOfInput)
    --> Ok False

-}
position : Parser context error Position
position =
    Parser <| \state -> Good (Position state.offset) state


{-| Position signifying the start of input.

This is mostly useful when feeding absolute offsets to
[`randomAccess`](#randomAccess).

-}
startOfInput : Position
startOfInput =
    Position 0


{-| Read some data based on an offset.

This is meant for "out of band" reading - the resulting parser will resume
reading where you left off.

As an example, consider we have some data like this:

  - An integer specifying the length of a string
  - An offset to the string, relative to the entire sequence
  - A number we also need

Which can be represented like so:

    import Bytes exposing (Bytes)
    import Bytes.Encode as E

    input : Bytes
    input =
        [ E.unsignedInt8 5 -- length of the string we're interested in
        , E.unsignedInt8 15 -- absolute offset to the string
        , E.unsignedInt8 6 -- another number we're interested in
        , E.string (String.repeat 12 "\u{0000}") -- buffer. Its content is irrelevant.
        , E.string "hello" -- our actual string
        ]
            |> E.sequence
            |> E.encode

Now, to decode this, let's first try decoding the `String` by decoding the
length and offset, and then reading the data:

    import Bytes.Parser as P exposing (Parser)


    string : Parser c e String
    string =
        P.succeed Tuple.pair
            |> P.keep P.unsignedInt8
            |> P.keep P.unsignedInt8
            |> P.andThen readStringWithLengthAndOffset


    readStringWithLengthAndOffset : ( Int, Int ) -> Parser c e String
    readStringWithLengthAndOffset ( length, offset ) =
        P.randomAccess
            { offset = offset, relativeTo = P.startOfInput }
            (P.string length)


    P.run string input
    --> Ok "hello"

Now, to illustrate the "resume" behaviour, let's use the above parser, and also
read the interesting number:

    final : Parser c e { string : String, number : Int }
    final =
        P.succeed (\s n -> { string = s, number = n })
            |> P.keep string
            |> P.keep P.unsignedInt8


    P.run final input
    --> Ok { string = "hello", number = 6 }

The trick here is that parsing continues its sequential behaviour, with the
`randomAccess` parser running in a separate context.

If the offset isn't absolute, but relative, we can use a similar setup, with the
addition of specifying the position we want the offset to be relative to using
[`position`](#position).

    relativeString : Parser c e String
    relativeString =
        P.succeed readRelativeString
            |> P.keep P.unsignedInt8
            |> P.keep P.position
            |> P.keep P.unsignedInt8
            |> P.andThen identity

    readRelativeString : Int -> P.Position -> Int -> Parser c e String
    readRelativeString length marker offset =
        P.randomAccess
            { offset = offset, relativeTo = marker }
            (P.string length)

-}
randomAccess :
    { offset : Int, relativeTo : Position }
    -> Parser context error value
    -> Parser context error value
randomAccess config (Parser f) =
    Parser <|
        \state ->
            let
                (Position start) =
                    config.relativeTo
            in
            case f { state | offset = start + config.offset } of
                Good v newState ->
                    Good v { newState | offset = state.offset }

                Bad e ->
                    Bad e


{-| A Parser that always fails with the given error.

    import Bytes.Encode as E
    import Bytes.Parser as P


    type Error = SomeFailure


    E.sequence []
        |> E.encode
        |> P.run (P.fail SomeFailure)
    --> Err (P.Custom SomeFailure)

-}
fail : error -> Parser context error value
fail e =
    Parser <| \_ -> Bad (Custom e)


{-| Add context to errors that may occur during parsing.

Adding context makes it easier to debug where issues occur.

    import Bytes.Encode as E
    import Bytes.Parser as P


    type Context = Header | DataArea


    E.sequence []
        |> E.encode
        |> P.run (P.inContext Header P.unsignedInt8)
    --> Err (P.InContext Header P.EndOfInput)

-}
inContext :
    context
    -> Parser context error value
    -> Parser context error value
inContext ctx (Parser f) =
    Parser
        (\state ->
            case f state of
                Good v s ->
                    Good v s

                Bad e ->
                    Bad (InContext ctx e)
        )


{-| Run the given parser on the provided bytes and the result.

    import Bytes.Encode as E
    import Bytes.Parser as P


    E.string "hello"
        |> E.encode
        |> P.run (P.string 5)
    --> Ok "hello"


    E.string "hello"
        |> E.encode
        |> P.run (P.string 6)
    --> Err P.EndOfInput

-}
run :
    Parser context error value
    -> Bytes
    -> Result (Error context error) value
run (Parser f) input =
    let
        initialState : State
        initialState =
            { offset = 0
            , stack = []
            , input = input
            }
    in
    case f initialState of
        Good v _ ->
            Ok v

        Bad e ->
            Err e


{-| Transform the value a parser produces

    import Bytes.Encode as E
    import Bytes.Parser as P


    E.string "hello"
        |> E.encode
        |> P.run (P.map String.length (P.string 5))
    --> Ok 5

-}
map :
    (a -> b)
    -> Parser context error a
    -> Parser context error b
map t (Parser f) =
    Parser <|
        \state ->
            case f state of
                Good v s ->
                    Good (t v) s

                Bad e ->
                    Bad e


{-| Parse one thing, and then parse another thing based on the first thing.

This is very useful to make the content of your data drive your parser. As an
example, consider a string encoded as the length of the string, followed by the
actual data:

    import Bytes.Encode as E
    import Bytes.Parser as P exposing (Parser)


    string : Parser c e String
    string =
        P.unsignedInt8 |> P.andThen P.string


    [ E.unsignedInt8 5
    , E.string "hello"
    ]
        |> E.sequence
        |> E.encode
        |> P.run string
    --> Ok "hello"

-}
andThen :
    (a -> Parser context error b)
    -> Parser context error a
    -> Parser context error b
andThen toParserB (Parser f) =
    Parser <|
        \state ->
            case f state of
                Good v s ->
                    let
                        (Parser p) =
                            toParserB v
                    in
                    p s

                Bad e ->
                    Bad e


{-| Combine what 2 parsers produce into a single parser.

    import Bytes exposing (Bytes)
    import Bytes.Encode as E
    import Bytes.Parser as P exposing (Parser)


    input : Bytes
    input =
        [ E.unsignedInt8 3
        , E.string "wat"
        ]
            |> E.sequence
            |> E.encode


    map2Example : Parser c e String
    map2Example =
        P.map2 String.repeat P.unsignedInt8 (P.string 3)


    P.run map2Example input
    --> Ok "watwatwat"

Note that the effect of `map2` (and, in fact, every `map` variation) can also be
achieved using a combination of [`succeed`](#succeed) and [`keep`](#keep).

    equivalent : Parser c e String
    equivalent =
        P.succeed String.repeat
            |> P.keep P.unsignedInt8
            |> P.keep (P.string 3)

    P.run equivalent input
    --> Ok "watwatwat"

-}
map2 :
    (x -> y -> z)
    -> Parser context error x
    -> Parser context error y
    -> Parser context error z
map2 f parserX parserY =
    parserX |> andThen (\x -> parserY |> andThen (\y -> succeed (f x y)))


{-| -}
map3 :
    (w -> x -> y -> z)
    -> Parser context error w
    -> Parser context error x
    -> Parser context error y
    -> Parser context error z
map3 f parserW parserX parserY =
    map2 f parserW parserX
        |> keep parserY


{-| -}
map4 :
    (v -> w -> x -> y -> z)
    -> Parser context error v
    -> Parser context error w
    -> Parser context error x
    -> Parser context error y
    -> Parser context error z
map4 f parserV parserW parserX parserY =
    map3 f parserV parserW parserX
        |> keep parserY


{-| -}
map5 :
    (u -> v -> w -> x -> y -> z)
    -> Parser context error u
    -> Parser context error v
    -> Parser context error w
    -> Parser context error x
    -> Parser context error y
    -> Parser context error z
map5 f parserU parserV parserW parserX parserY =
    map4 f parserU parserV parserW parserX
        |> keep parserY


{-| Keep the value produced by a parser in a pipeline.

Together with [`succeed`](#succeed) and [`ignore`](#ignore), this allows writing
pretty flexible parsers in a straightforward manner: the order in which things
are parsed is apparent.

    import Bytes.Encode as E
    import Bytes.Parser as P exposing (Parser)

    parser : Parser c e (Int, Int)
    parser =
        P.succeed Tuple.pair
            |> P.keep P.unsignedInt8
            |> P.ignore P.unsignedInt8
            |> P.keep P.unsignedInt8

    [ E.unsignedInt8 12
    , E.unsignedInt8 3
    , E.unsignedInt8 45
    ]
        |> E.sequence
        |> E.encode
        |> P.run parser
    --> Ok ( 12, 45 )

-}
keep :
    Parser context error a
    -> Parser context error (a -> b)
    -> Parser context error b
keep val fun =
    map2 (<|) fun val


{-| Ignore the value produced by a parser.

Note that the parser must still succeed for the pipeline to succeed. This means
you can use this for checking the value of something, without using the value.

    import Bytes.Encode as E
    import Bytes.Parser as P exposing (Parser)


    type Error = Mismatch { expected : Int, actual : Int }


    match : Int -> Parser c Error Int
    match expected =
        P.unsignedInt8
            |> P.andThen
                (\actual ->
                    if expected == actual then
                        P.succeed actual
                    else
                        P.fail (Mismatch { expected = expected, actual = actual})
                )

    parser : Parser c Error ()
    parser =
        P.succeed ()
            |> P.ignore (match 66)


    E.unsignedInt8 66
        |> E.encode
        |> P.run parser
    --> Ok ()


    E.unsignedInt8 44
        |> E.encode
        |> P.run parser
    --> Mismatch { expected = 66, actual = 44 }
    -->   |> P.Custom
    -->   |> Err

-}
ignore :
    Parser context error ignore
    -> Parser context error keep
    -> Parser context error keep
ignore skipper keeper =
    map2 always keeper skipper


{-| Skip a number of bytes in a pipeline.

This is similar to `ignore`, but rather than parsing a value and discarding it,
this just goes ahead and skips them altogether.

-}
skip : Int -> Parser context error value -> Parser context error value
skip nBytes =
    ignore (bytes nBytes)


{-| Tries a bunch of parsers and succeeds with the first one to succeed.

Note that this uses backtracking when a parser fails after making some progress.

-}
oneOf : List (Parser context error value) -> Parser context error value
oneOf options =
    Parser (oneOfHelp options [])


oneOfHelp :
    List (Parser context error value)
    -> List (Error context error)
    -> State
    -> ParseResult context error value
oneOfHelp options errors state =
    case options of
        [] ->
            Bad (BadOneOf (List.reverse errors))

        (Parser f) :: xs ->
            case f state of
                Good v s ->
                    Good v s

                Bad e ->
                    oneOfHelp xs (e :: errors) state


{-| Represent the next step of a loop: Either continue looping with some new
internal state, or finish while producing a value.
-}
type Step state a
    = Loop state
    | Done a


{-| Loop a parser until it declares it is done looping.

The first argument is a function which, given some state, will usually parse
some stuff and indicate it wants to either continue, or declare it is done and
produce the final value. The second argument is the initial state for the loop.

This particular order of parameters was chosen to make it somewhat easier to
produce the initial state using a parser (which seems to be a fairly common use
case) and to hint at the mental model, which isn't unlike a `fold`.

    import Bytes.Encode as E
    import Bytes.Parser as P

    nullTerminatedString_ : (Int, P.Position) -> P.Parser c e (P.Step (Int, P.Position) String)
    nullTerminatedString_ ( count, startPos ) =
        P.unsignedInt8
            |> P.andThen
                (\byte ->
                     if byte == 0x00 then
                         P.string count
                             |> P.randomAccess { offset = 0, relativeTo = startPos }
                             |> P.map P.Done
                     else
                         P.succeed (P.Loop ( count + 1, startPos ))
                )

    nullTerminatedString : Parser c e String
    nullTerminatedString =
        P.map (Tuple.pair 0) P.position
            |> P.andThen (P.loop nullTerminatedString_)


    [ E.string "hello world!"
    , E.unsignedInt8 0
    ]
        |> E.sequence
        |> E.encode
        |> P.run nullTerminatedString
    --> Ok "hello world!"

-}
loop :
    (state -> Parser context error (Step state a))
    -> state
    -> Parser context error a
loop toNext initialState =
    Parser (loopHelp initialState toNext)


loopHelp :
    state
    -> (state -> Parser context error (Step state a))
    -> State
    -> ParseResult context error a
loopHelp loopState toNext state =
    let
        (Parser next) =
            toNext loopState
    in
    case next state of
        Good (Loop newLoopState) newState ->
            loopHelp newLoopState toNext newState

        Good (Done v) newState ->
            Good v newState

        Bad e ->
            Bad e


{-| Repeat a given parser `count` times.

The order of arguments is based on the common occurence of reading the number of
times to repeat something through a parser.

    import Bytes.Encode as E
    import Bytes.Parser as P


    intList : P.Parser c e (List Int)
    intList =
        P.unsignedInt8 |> P.andThen (P.repeat P.unsignedInt8)


    [ 5, 0, 1, 2, 3, 4 ]
        |> List.map E.unsignedInt8
        |> E.sequence
        |> E.encode
        |> P.run intList
    --> Ok [ 0, 1, 2, 3, 4 ]

-}
repeat : Parser context error value -> Int -> Parser context error (List value)
repeat p nTimes =
    loop (repeatHelp p) ( nTimes, [] )


repeatHelp :
    Parser context error value
    -> ( Int, List value )
    -> Parser context error (Step ( Int, List value ) (List value))
repeatHelp p ( cnt, acc ) =
    if cnt <= 0 then
        succeed (Done (List.reverse acc))

    else
        map (\v -> Loop ( cnt - 1, v :: acc )) p



-- Basics


{-| Parse one byte into an integer from 0 to 255.
-}
unsignedInt8 : Parser context error Int
unsignedInt8 =
    fromDecoder Decode.unsignedInt8 1


{-| Parse one byte into an integer from -128 to 127.
-}
signedInt8 : Parser context error Int
signedInt8 =
    fromDecoder Decode.signedInt8 1


{-| Parse two bytes into an integer from 0 to 65535.
-}
unsignedInt16 : Bytes.Endianness -> Parser context error Int
unsignedInt16 bo =
    fromDecoder (Decode.unsignedInt16 bo) 2


{-| Parse two bytes into an integer from -32768 to 32767.
-}
signedInt16 : Bytes.Endianness -> Parser context error Int
signedInt16 bo =
    fromDecoder (Decode.signedInt16 bo) 2


{-| Parse four bytes into an integer from 0 to 4294967295.
-}
unsignedInt32 : Bytes.Endianness -> Parser context error Int
unsignedInt32 bo =
    fromDecoder (Decode.unsignedInt32 bo) 4


{-| Parse four bytes into an integer from -2147483648 to 2147483647.
-}
signedInt32 : Bytes.Endianness -> Parser context error Int
signedInt32 bo =
    fromDecoder (Decode.signedInt32 bo) 4


{-| Parse 4 bytes into a Float.
-}
float32 : Bytes.Endianness -> Parser context error Float
float32 bo =
    fromDecoder (Decode.float32 bo) 4


{-| Parse 8 bytes into a Float.
-}
float64 : Bytes.Endianness -> Parser context error Float
float64 bo =
    fromDecoder (Decode.float64 bo) 8


{-| Parse `count` bytes as `Bytes`.
-}
bytes : Int -> Parser context error Bytes
bytes count =
    fromDecoder (Decode.bytes count) count


{-| Parse `count` bytes representing UTF-8 characters into a String.

Note that Elm strings use UTF-16. As a result, the `String.length` will not
always agree with the number of bytes that went into it!

    import Bytes.Encode as E
    import Bytes.Parser as P


    [ 0xF0, 0x9F, 0x91, 0x8D ]
        |> List.map E.unsignedInt8
        |> E.sequence
        |> E.encode
        |> P.run (P.string 4)
    --> Ok "👍"

-}
string : Int -> Parser context error String
string byteCount =
    fromDecoder (Decode.string byteCount) byteCount


fromDecoder : Decoder v -> Int -> Parser context error v
fromDecoder dec byteLength =
    Parser <|
        \state ->
            let
                combined : Decoder v
                combined =
                    Decode.map2 (always identity) (Decode.bytes state.offset) dec
            in
            case Decode.decode combined state.input of
                Just res ->
                    Good res { state | offset = state.offset + byteLength }

                Nothing ->
                    Bad EndOfInput
