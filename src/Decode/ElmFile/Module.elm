module Decode.ElmFile.Module exposing (name)

import ElmFile.Module
import Bytes.Decode as Decode
import Decode.ElmFile.Package
import Decode.Util
import Decode.Util.Decode64 exposing (Decoder64)


name : Decoder64 ElmFile.Module.Name
name =
    Decode.map2
        (Result.map2
            (\package module_ ->
                ElmFile.Module.Name
                    { package = package
                    , module_ = module_
                    }
            )
        )
        Decode.ElmFile.Package.name
        Decode.Util.name
