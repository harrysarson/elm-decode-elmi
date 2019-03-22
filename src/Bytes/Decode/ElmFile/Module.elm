module Bytes.Decode.ElmFile.Module exposing (name)

import ElmFile.Module
import Bytes.Decode as Decode
import Bytes.Decode.ElmFile.Package
import Bytes.Decode.Util
import Bytes.Decode.Util.Decode64 exposing (Decoder64)


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
        Bytes.Decode.ElmFile.Package.name
        Bytes.Decode.Util.name
