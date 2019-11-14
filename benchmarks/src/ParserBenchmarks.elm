module ParserBenchmarks exposing (..)

import Bytes.Parser as Parser exposing (Error, Parser, Position, Step(..))
import Data


decodeList : () -> Result (Error context error) (List Float)
decodeList =
    \_ -> Parser.run parseFloatList Data.bytesFloatList


decodeLongList : () -> Result (Error context error) (List Float)
decodeLongList =
    \_ -> Parser.run parseFloatList Data.longBytesFloatList


parseFloatList : Parser context error (List Float)
parseFloatList =
    list (Parser.float64 Data.endianness)


list : Parser context error a -> Parser context error (List a)
list decoder =
    Parser.unsignedInt32 Data.endianness
        |> Parser.andThen (\count -> Parser.loop (listStep decoder) ( count, [] ))


listStep :
    Parser context error a
    -> ( Int, List a )
    -> Parser context error (Step ( Int, List a ) (List a))
listStep decoder ( n, list_ ) =
    if n <= 0 then
        Parser.succeed (Done list_)

    else
        Parser.map (\x -> Loop ( n - 1, x :: list_ )) decoder
