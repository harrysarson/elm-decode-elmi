module Decode.Util exposing (decodeDict, name, list, listLength)

import Bytes.Decode as Decode
import Decode.Util.Decode64 as Decode64 exposing (Decoder64)
import Dict exposing (Dict)
import Result.Extra


name : Decoder64 String
name =
    Decode64.uint64
        |> Decode.andThen
            (\countR ->
                countR
                    |> Result.map (Decode.string >> Decode.map Ok)
                    |> Result.Extra.extract (\e -> Decode.succeed (Err e))
            )


decodeDict : Decoder64 comparable -> Decoder64 b -> Decoder64 (Dict comparable b)
decodeDict keyDecoder valueDecoder =
    Decode64.uint64
        |> Decode64.andThen
            (\len -> Decode64.fold
                (\( k, v ) dict -> Dict.insert k v dict)
                Dict.empty
                len
                (Decode.map2 (Result.map2 Tuple.pair) keyDecoder valueDecoder)
            )

list : Decoder64 a -> Decoder64 (List a)
list d =
    Decode64.uint64
        |> Decode64.andThen (\n -> listLength n d)


listLength : Int -> Decoder64 a -> Decoder64 (List a)
listLength n decoder =
    Decode64.fold (::) [] n decoder
