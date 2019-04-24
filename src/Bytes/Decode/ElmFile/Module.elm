module Bytes.Decode.ElmFile.Module exposing (name)

{-|

@docs name

-}

import Bytes.Decode as Decode exposing (Decoder)
import Bytes.Decode.ElmFile.Package
import Bytes.Decode.Util
import ElmFile.Module


{-| Decoder for the name of an elm module.
-}
name : (Int -> Int -> any) -> Decoder ElmFile.Module.Name
name cb =
    Decode.map2
        (\package module_ ->
            ElmFile.Module.Name
                { package = package
                , module_ = module_
                }
        )
        (Bytes.Decode.ElmFile.Package.name cb)
        (Bytes.Decode.Util.name cb)
