module Bytes.Decode.Util exposing (decodeDict, int64, list, listLength, name, uint64)

import Bytes
import Bytes.Decode as Decode exposing (Decoder)
import Dict exposing (Dict)
import Result.Extra


uint64 : (Int -> Int -> any) -> Decoder Int
uint64 cb =
    Decode.unsignedInt32 Bytes.BE
        |> Decode.andThen
            (\upper ->
                let
                    lowerDecoder =
                        Decode.unsignedInt32 Bytes.BE
                in
                if upper == 0 then
                    lowerDecoder

                else
                    let
                        _ =
                            Decode.map (cb upper) lowerDecoder
                    in
                    Decode.fail
            )


int64 : (Int -> Int -> any) -> Decoder Int
int64 cb =
    Decode.signedInt32 Bytes.BE
        |> Decode.andThen
            (\upper ->
                let
                    lowerDecoder =
                        Decode.signedInt32 Bytes.BE
                in
                if upper == 0 then
                    lowerDecoder

                else if upper == -1 then
                    lowerDecoder
                        |> Decode.andThen
                            (\lower ->
                                if lower < 0 then
                                    Decode.succeed lower

                                else
                                    let
                                        _ =
                                            cb upper lower
                                    in
                                    Decode.fail
                            )

                else
                    let
                        _ =
                            Decode.map (cb upper) lowerDecoder
                    in
                    Decode.fail
            )


name : (Int -> Int -> any) -> Decoder String
name cb =
    uint64 cb
        |> Decode.andThen Decode.string


decodeDict : Decoder comparable -> Decoder b -> (Int -> Int -> any) -> Decoder (Dict comparable b)
decodeDict keyDecoder valueDecoder cb =
    uint64 cb
        |> Decode.andThen
            (\len ->
                fold
                    (\( k, v ) dict -> Dict.insert k v dict)
                    Dict.empty
                    len
                    (Decode.map2 Tuple.pair keyDecoder valueDecoder)
            )


list : Decoder a -> (Int -> Int -> any) -> Decoder (List a)
list d cb =
    uint64 cb
        |> Decode.andThen (\n -> listLength n d)


listLength : Int -> Decoder a -> Decoder (List a)
listLength n decoder =
    fold (::) [] n decoder


foldStep : (a -> b -> b) -> Decoder a -> ( Int, b ) -> Decoder (Decode.Step ( Int, b ) b)
foldStep func d ( n, acc ) =
    if n <= 0 then
        Decode.succeed (Decode.Done acc)

    else
        Decode.map
            (\x -> Decode.Loop ( n - 1, func x acc ))
            d


fold : (a -> b -> b) -> b -> Int -> Decoder a -> Decoder b
fold func initial length d =
    Decode.loop
        ( length, initial )
        (foldStep func d)
