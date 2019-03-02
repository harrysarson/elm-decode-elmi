module Main exposing (Flags, init, main)

import Ast.Canonical
import Base64
import Bytes.Decode
import Bytes.Encode
import Decode.ElmFile.Interface
import Dict
import Platform


type alias Flags =
    { elmi : String
    }


init : Flags -> ( (), Cmd never )
init flags =
    let
        elmi =
            Base64.toBytes (Debug.log "base64" flags.elmi)
                |> Maybe.withDefault (Bytes.Encode.encode <| Bytes.Encode.unsignedInt8 0)

        interface =
            Bytes.Decode.decode Decode.ElmFile.Interface.interface elmi

        _ =
            Debug.log "hi" interface
    in
    ( (), Cmd.none )


main : Platform.Program Flags () msg
main =
    Platform.worker
        { init = init
        , update = always <| always ( (), Cmd.none )
        , subscriptions = always Sub.none
        }
