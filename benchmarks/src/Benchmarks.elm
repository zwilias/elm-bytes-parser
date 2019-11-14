module Benchmarks exposing (..)

import Benchmark exposing (Benchmark)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import BytesBenchmarks
import ParserBenchmarks


elmBytes =
    "elm/bytes"


zwiliasElmBytesParser =
    "zwilias/elm-bytes-parser"


main : BenchmarkProgram
main =
    program <|
        Benchmark.describe (elmBytes ++ " vs " ++ zwiliasElmBytesParser)
            [ Benchmark.compare
                "Decode list with 10 floats"
                elmBytes
                BytesBenchmarks.decodeList
                zwiliasElmBytesParser
                ParserBenchmarks.decodeList
            , Benchmark.compare
                "Decode list with 1000 floats"
                elmBytes
                BytesBenchmarks.decodeLongList
                zwiliasElmBytesParser
                ParserBenchmarks.decodeLongList
            ]
