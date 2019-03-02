module Decode.ElmFile.Package exposing (name)

import ElmFile.Package
import Bytes.Decode as Decode
import Decode.Util
import Decode.Util.Decode64 exposing (Decoder64)


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
        Decode.Util.name
        Decode.Util.name
