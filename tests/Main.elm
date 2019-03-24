module Main exposing (basic, context, loop, repeat)

import Bytes as B
import Bytes.Encode as E
import Bytes.Parser as P
import Expect
import Test exposing (..)


basic : Test
basic =
    describe "very basic stuff"
        [ test "basic unsignedInt8 works" <|
            \_ ->
                E.unsignedInt8 8
                    |> E.encode
                    |> P.run P.unsignedInt8
                    |> Expect.equal (Ok 8)
        , test "no reading past end of input" <|
            \_ ->
                P.run P.unsignedInt8 emptyBytes
                    |> Expect.equal (Err (P.OutOfBounds { at = 0, bytes = 1 }))
        , test "succeed succeeds" <|
            \_ ->
                P.run (P.succeed "sure") emptyBytes
                    |> Expect.equal (Ok "sure")
        , test "fail fails" <|
            \_ ->
                P.run (P.fail "nope") emptyBytes
                    |> Expect.equal (Err (P.Custom { at = 0 } "nope"))
        , test "inContext adds context" <|
            \_ ->
                P.run (P.inContext "context" P.unsignedInt8) emptyBytes
                    |> Expect.equal
                        (P.OutOfBounds { at = 0, bytes = 1 }
                            |> P.InContext { label = "context", start = 0 }
                            |> Err
                        )
        , test "can read multiple things" <|
            \_ ->
                encodeSequence
                    [ E.unsignedInt8 1
                    , E.unsignedInt8 2
                    ]
                    |> P.run (P.map2 Tuple.pair P.unsignedInt8 P.unsignedInt8)
                    |> Expect.equal (Ok ( 1, 2 ))
        ]


loop : Test
loop =
    let
        parser : P.Parser c e (List String)
        parser =
            P.unsignedInt8
                |> P.andThen (\cnt -> P.loop loopHelper ( cnt, [] ))

        loopHelper :
            ( Int, List String )
            -> P.Parser c e (P.Step ( Int, List String ) (List String))
        loopHelper ( cnt, acc ) =
            if cnt <= 0 then
                P.succeed (P.Done (List.reverse acc))

            else
                P.string 3
                    |> P.map (\s -> P.Loop ( cnt - 1, s :: acc ))
    in
    describe "loops"
        [ test "When everything goes well" <|
            \_ ->
                encodeSequence
                    [ E.unsignedInt8 3
                    , E.string "foo"
                    , E.string "bar"
                    , E.string "baz"
                    ]
                    |> P.run parser
                    |> Expect.equal (Ok [ "foo", "bar", "baz" ])
        , test "failure propagates" <|
            \_ ->
                encodeSequence
                    [ E.unsignedInt8 3
                    , E.string "foo"
                    , E.string "bar"
                    ]
                    |> P.run parser
                    |> Expect.equal (Err (P.OutOfBounds { at = 7, bytes = 3 }))
        ]


repeat : Test
repeat =
    test "repeat repeats" <|
        \_ ->
            let
                parser : P.Parser c e (List String)
                parser =
                    P.andThen (P.repeat (P.string 3)) P.unsignedInt8
            in
            encodeSequence
                [ E.unsignedInt8 3
                , E.string "foo"
                , E.string "bar"
                , E.string "baz"
                ]
                |> P.run parser
                |> Expect.equal (Ok [ "foo", "bar", "baz" ])


context : Test
context =
    let
        stream : P.Parser String e (List String)
        stream =
            P.unsignedInt8
                |> P.andThen (P.repeat string)
                |> P.inContext "stream"

        string : P.Parser String e String
        string =
            P.unsignedInt8
                |> P.andThen P.string
                |> P.inContext "string"
    in
    describe "context"
        [ test "parser is actually correct" <|
            \_ ->
                encodeSequence
                    [ E.unsignedInt8 2
                    , E.unsignedInt8 3
                    , E.string "foo"
                    , E.unsignedInt8 3
                    , E.string "bar"
                    ]
                    |> P.run stream
                    |> Expect.equal (Ok [ "foo", "bar" ])
        , test "Context stacks" <|
            \_ ->
                encodeSequence
                    [ E.unsignedInt8 2
                    , E.unsignedInt8 3
                    , E.string "foo"
                    , E.unsignedInt8 4
                    , E.string "bar"
                    ]
                    |> P.run stream
                    |> Expect.equal
                        (P.OutOfBounds { at = 6, bytes = 4 }
                            |> P.InContext { label = "string", start = 5 }
                            |> P.InContext { label = "stream", start = 0 }
                            |> Err
                        )
        ]


encodeSequence : List E.Encoder -> B.Bytes
encodeSequence =
    E.sequence >> E.encode


emptyBytes : B.Bytes
emptyBytes =
    encodeSequence []
