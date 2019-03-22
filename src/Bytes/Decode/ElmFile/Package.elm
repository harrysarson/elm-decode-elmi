module Bytes.Decode.ElmFile.Package exposing (name)

import ElmFile.Package
import Bytes.Decode as Decode exposing (Decoder)
import Bytes.Decode.Util


name : (Int -> Int -> any) -> Decoder ElmFile.Package.Name
name cb =
    Decode.map2
        (\author project ->
            ElmFile.Package.Name
                { author = author
                , project = project
                }
        )
        (Bytes.Decode.Util.name cb)
        (Bytes.Decode.Util.name cb)
