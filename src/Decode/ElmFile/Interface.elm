module Decode.ElmFile.Interface exposing (alias_, interface, union)

import Ast.Canonical
import ElmFile.Interface exposing (Interface)
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
import Result.Extra
import Set exposing (Set)


interface : Decoder64 Interface
interface =
    Decode.map3
        (Result.map3
            (\d u a ->
                ElmFile.Interface.Interface
                    { types_ = d
                    , unions = u
                    , aliases = a
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
