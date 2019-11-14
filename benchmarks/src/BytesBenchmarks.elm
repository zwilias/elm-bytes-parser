module BytesBenchmarks exposing (decodeList, decodeLongList)

import Bytes.Decode
import Data


decodeFloatList : Bytes.Decode.Decoder (List Float)
decodeFloatList =
    list (Bytes.Decode.float64 Data.endianness)


decodeList : () -> Maybe (List Float)
decodeList =
    \_ -> Bytes.Decode.decode decodeFloatList Data.bytesFloatList


decodeLongList : () -> Maybe (List Float)
decodeLongList =
    \_ -> Bytes.Decode.decode decodeFloatList Data.bytesFloatList


list : Bytes.Decode.Decoder a -> Bytes.Decode.Decoder (List a)
list decoder =
    Bytes.Decode.unsignedInt32 Data.endianness
        |> Bytes.Decode.andThen (\len -> Bytes.Decode.loop ( len, [] ) (listStep decoder))


listStep : Bytes.Decode.Decoder a -> ( Int, List a ) -> Bytes.Decode.Decoder (Bytes.Decode.Step ( Int, List a ) (List a))
listStep decoder ( n, xs ) =
    if n <= 0 then
        Bytes.Decode.succeed (Bytes.Decode.Done xs)

    else
        Bytes.Decode.map (\x -> Bytes.Decode.Loop ( n - 1, x :: xs )) decoder
