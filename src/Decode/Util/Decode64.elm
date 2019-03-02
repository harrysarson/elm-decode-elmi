module Decode.Util.Decode64 exposing (Decoder64, Error(..), andThen, fold, uint64)

import Bytes
import Bytes.Decode as Decode exposing (Decoder)
import Result.Extra


type Error
    = InterfaceCountTooHigh Int


type alias Decoder64 a =
    Decoder (Result Error a)


uint64 : Decoder64 Int
uint64 =
    Decode.unsignedInt32 Bytes.BE
        |> Decode.andThen
            (\value ->
                if value /= 0 then
                    Decode.succeed (Err <| InterfaceCountTooHigh value)

                else
                    Decode.unsignedInt32 Bytes.BE
                        |> Decode.map Ok
            )


andThen : (a -> Decoder64 b) -> Decoder64 a -> Decoder64 b
andThen fn =
    Decode.andThen
        (Result.map fn >> Result.Extra.extract (\e -> Decode.succeed (Err e)))


foldStep : (a -> b -> b) -> Decoder64 a -> ( Int, b ) -> Decoder (Decode.Step ( Int, b ) (Result Error b))
foldStep func d ( n, acc ) =
    if n <= 0 then
        Decode.succeed (Decode.Done (Ok acc))

    else
        Decode.map
            (Result.map (\x -> Decode.Loop ( n - 1, func x acc ))
                >> Result.Extra.extract (\e -> Decode.Done (Err e))
            )
            d


fold : (a -> b -> b) -> b -> Decoder64 a -> Int -> Decoder64 b
fold func initial d n =
    Decode.loop
        ( n, initial )
        (foldStep func d)
