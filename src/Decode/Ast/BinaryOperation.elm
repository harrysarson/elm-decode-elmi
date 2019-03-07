module Decode.Ast.BinaryOperation exposing (..)

import Decode.Util.Decode64 as Decode64 exposing (Decoder64)
import Bytes.Decode as Decode
import Ast.BinaryOperation

precedence : Decoder64 Ast.BinaryOperation.Precedence
precedence =
    Decode64.int64
        |> Decode.map (Result.map Ast.BinaryOperation.Precedence)

associativity : Decoder64 Ast.BinaryOperation.Associativity
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
        |> Decode.map Ok
