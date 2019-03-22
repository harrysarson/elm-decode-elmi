module Bytes.Decode.Util.Decode64 exposing (Decoder64, Error(..), andThen, fold, uint64, int64)

import Bytes
import Bytes.Decode as Decode exposing (Decoder)
import Result.Extra
import Bitwise


type Error
    = InterfaceCountTooHigh Int


type alias Decoder64 a =
    Decoder (Result Error a)


uint64 : Decoder64 Int
uint64 =
    Decode.unsignedInt32 Bytes.BE
        |> Decode.andThen
            (\value ->
                if value == 0 then
                    Decode.unsignedInt32 Bytes.BE
                        |> Decode.map Ok

                else
                    Decode.succeed (Err <| InterfaceCountTooHigh value)
            )


int64 : Decoder64 Int
int64 =
    Decode.signedInt32 Bytes.BE
        |> Decode.andThen
            (\value ->
                if value == 0 then
                    Decode.signedInt32 Bytes.BE
                        |> Decode.map Ok

                else if value == -1 then
                    Decode.signedInt32 Bytes.BE
                        |> Decode.map (\val ->
                            if val < 0 then
                                Ok(val)
                            else Err <| InterfaceCountTooHigh -1)

                else
                    Decode.succeed (Err <| InterfaceCountTooHigh value)
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


fold : (a -> b -> b) -> b -> Int -> Decoder64 a -> Decoder64 b
fold func initial n d =
    Decode.loop
        ( n, initial )
        (foldStep func d)
