module Decode.ElmFile.Interface exposing (interface, interfaces)

import Ast.Canonical
import ElmFile.Interface exposing (Interface, Interfaces)
import ElmFile.Module
import Bytes
import Bytes.Decode as Decode exposing (Decoder)
import Bytes.Encode as Encode exposing (Encoder)
import Decode.Ast.Canonical
import Decode.ElmFile.Module
import Decode.Util
import Decode.Util.Decode64 as Decode64 exposing (Decoder64, uint64)
import Dict exposing (Dict)
import Maybe.Extra
import  Ast.BinaryOperation
import Decode.Ast.BinaryOperation
import Result.Extra
import Set exposing (Set)


interfaces : Decoder64 Interfaces
interfaces =
    Decode.map
        (Result.map ElmFile.Interface.Interfaces)
        (Decode.Util.list <|
            Decode.map2 (Result.map2 Tuple.pair) (Decode.ElmFile.Module.name) interface
        )


interface : Decoder64 Interface
interface =
    Decode.map4
        (Result.map4
            (\d u a b ->
                ElmFile.Interface.Interface
                    { types_ = d
                    , unions = u
                    , aliases = a
                    , binaryOperations = b
                    }
            )
        )
        (Decode.Util.decodeDict
            Decode.Util.name
            Decode.Ast.Canonical.annotation
        )
        (Decode.Util.decodeDict
            Decode.Util.name
            union
        )
        (Decode.Util.decodeDict
            Decode.Util.name
            alias_
        )
        (Decode.Util.decodeDict
            Decode.Util.name
            binaryOperation
        )


union : Decoder64 ElmFile.Interface.Union
union =
    Decode.unsignedInt8
        |> Decode.andThen
            (\id ->
                case id of
                    0 ->
                        Decode.Ast.Canonical.union |> Decode.map (Result.map ElmFile.Interface.OpenUnion)

                    1 ->
                        Decode.Ast.Canonical.union |> Decode.map (Result.map ElmFile.Interface.ClosedUnion)

                    2 ->
                        Decode.Ast.Canonical.union |> Decode.map (Result.map ElmFile.Interface.PrivateUnion)

                    _ ->
                        Decode.fail
            )


alias_ : Decoder64 ElmFile.Interface.Alias
alias_ =
    Decode.unsignedInt8
        |> Decode.andThen
            (\id ->
                case id of
                    0 ->
                        Decode.Ast.Canonical.alias_ |> Decode.map (Result.map ElmFile.Interface.PublicAlias)

                    1 ->
                        Decode.Ast.Canonical.alias_ |> Decode.map (Result.map ElmFile.Interface.PrivateAlias)

                    _ ->
                        Decode.fail
            )


binaryOperation : Decoder64 ElmFile.Interface.BinaryOperation
binaryOperation =
    Decode.map4
        (Result.map4
            (\n an ass p ->
                ElmFile.Interface.BinaryOperation
                    { name = n
                    , annotation = an
                    , associativity = ass
                    , precedence = p
                    }
            )
        )
        Decode.Util.name
        Decode.Ast.Canonical.annotation
        Decode.Ast.BinaryOperation.associativity
        Decode.Ast.BinaryOperation.precedence

