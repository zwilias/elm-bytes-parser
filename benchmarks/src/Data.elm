module Data exposing (bytesFloatList, endianness, longBytesFloatList)

{-| Data to be used by both elm/bytes and zwilias/elm-bytes-parser.
Make sure that these are constant values so that they aren't recomputed on each call.
-}

import Bytes
import Bytes.Encode
import Random


randomFloatList n =
    Random.list n (Random.float -100 100)


endianness =
    Bytes.BE


encodeFloatList floatList =
    Bytes.Encode.sequence
        [ Bytes.Encode.unsignedInt32 endianness (List.length floatList)
        , floatList |> List.map (Bytes.Encode.float64 endianness) |> Bytes.Encode.sequence
        ]


bytesFloatList : Bytes.Bytes
bytesFloatList =
    Random.step (randomFloatList 10) (Random.initialSeed 0) |> Tuple.first |> encodeFloatList |> Bytes.Encode.encode


longBytesFloatList : Bytes.Bytes
longBytesFloatList =
    Random.step (randomFloatList 1000) (Random.initialSeed 0) |> Tuple.first |> encodeFloatList |> Bytes.Encode.encode
