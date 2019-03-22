module Bytes.Decode.ElmFile.Package exposing (name)

import ElmFile.Package
import Bytes.Decode as Decode
import Bytes.Decode.Util
import Bytes.Decode.Util.Decode64 exposing (Decoder64)


name : Decoder64 ElmFile.Package.Name
name =
    Decode.map2
        (Result.map2
            (\author project ->
                ElmFile.Package.Name
                    { author = author
                    , project = project
                    }
            )
        )
        Bytes.Decode.Util.name
        Bytes.Decode.Util.name
