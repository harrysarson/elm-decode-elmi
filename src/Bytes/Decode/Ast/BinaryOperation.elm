module Bytes.Decode.Ast.BinaryOperation exposing (..)

import Bytes.Decode.Util.Decode64 as Decode64 exposing (Decoder64)
import Bytes.Decode as Decode exposing (Decoder)
import Ast.BinaryOperation
import Bytes.Decode.Util

precedence : (Int -> Int -> any) -> Decoder Ast.BinaryOperation.Precedence
precedence cb =
    Bytes.Decode.Util.int64 cb
        |> Decode.map (Ast.BinaryOperation.Precedence)

associativity : Decoder Ast.BinaryOperation.Associativity
associativity =
    Decode.unsignedInt8
        |> Decode.andThen
          (\id ->
              case id of
                  0 ->
                      Decode.succeed Ast.BinaryOperation.Left

                  1 ->
                      Decode.succeed Ast.BinaryOperation.None

                  2 ->
                      Decode.succeed Ast.BinaryOperation.Right

                  _ ->
                      Decode.fail
          )
