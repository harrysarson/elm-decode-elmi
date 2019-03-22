module Bytes.Decode.Ast.Canonical exposing (aliasType, alias_, annotation, fieldtype, type_, union, unionCtor, unionCtorOpts)

import Ast.Canonical
import ElmFile.Interface exposing (Interface)
import ElmFile.Module
import Bytes
import Bytes.Decode as Decode exposing (Decoder)
import Bytes.Encode as Encode exposing (Encoder)
import Bytes.Decode.ElmFile.Module
import Bytes.Decode.Util
import Bytes.Decode.Util.Decode64 as Decode64 exposing (Decoder64, uint64)
import Dict exposing (Dict)
import Maybe.Extra
import Result.Extra
import Set exposing (Set)


union : Decoder64 Ast.Canonical.Union
union =
    Decode.succeed
        (Result.map4
            (\vars alts numAlts opts ->
                Ast.Canonical.Union
                    { vars = vars
                    , alts = alts
                    , numAlts = numAlts
                    , opts = opts
                    }
            )
        )
        |> keeper (Bytes.Decode.Util.list Bytes.Decode.Util.name)
        |> keeper (Bytes.Decode.Util.list unionCtor)
        |> keeper uint64
        |> keeper unionCtorOpts


alias_ : Decoder64 Ast.Canonical.Alias
alias_ =
    Decode.succeed (Result.map2 Ast.Canonical.Alias)
        |> keeper (Bytes.Decode.Util.list Bytes.Decode.Util.name)
        |> keeper type_


unionCtor : Decoder64 Ast.Canonical.UnionCtor
unionCtor =
    Decode.succeed (Result.map4 Ast.Canonical.UnionCtor)
        |> keeper Bytes.Decode.Util.name
        |> keeper uint64
        |> keeper uint64
        |> keeper (Bytes.Decode.Util.list type_)


unionCtorOpts : Decoder64 Ast.Canonical.UnionCtorOpts
unionCtorOpts =
    Decode.unsignedInt8
        |> Decode.andThen
            (\id ->
                case id of
                    0 ->
                        Decode.succeed Ast.Canonical.CtorOptNormal

                    1 ->
                        Decode.succeed Ast.Canonical.CtorOptEnum

                    2 ->
                        Decode.succeed Ast.Canonical.CtorOptUnbox

                    _ ->
                        Decode.fail
            )
        |> Decode.map Ok


annotation : Decoder64 Ast.Canonical.Annotation
annotation =
    Decode.map2 (Result.map2 Ast.Canonical.Annotation)
        (Bytes.Decode.Util.decodeDict
            Bytes.Decode.Util.name
            (Decode.succeed <| Ok ())
            |> Decode.map (Result.map dictToSet)
        )
        type_


aliasType : Decoder64 Ast.Canonical.AliasType
aliasType =
    Decode.unsignedInt8
        |> Decode.andThen
            (\id ->
                case id of
                    0 ->
                        Decode.succeed (Result.map Ast.Canonical.Holey)
                            |> keeper type_

                    1 ->
                        Decode.succeed (Result.map Ast.Canonical.Filled)
                            |> keeper type_

                    _ ->
                        Decode.fail
            )


fieldtype : Decoder64 Ast.Canonical.FieldType
fieldtype =
    Decode.succeed (\a -> Result.map (Ast.Canonical.FieldType a))
        |> keeper (Decode.unsignedInt16 Bytes.BE)
        |> keeper type_


type_ : Decoder64 Ast.Canonical.Type
type_ =
    Decode.unsignedInt8
        |> Decode.andThen
            (\id ->
                case id of
                    0 ->
                        Decode.succeed (Result.map2 Ast.Canonical.TLambda)
                            |> keeper type_
                            |> keeper type_

                    1 ->
                        Decode.succeed (Result.map Ast.Canonical.TVar)
                            |> keeper Bytes.Decode.Util.name

                    2 ->
                        Decode.succeed (Result.map2 Ast.Canonical.TRecord)
                            |> keeper (Bytes.Decode.Util.decodeDict Bytes.Decode.Util.name fieldtype)
                            |> keeper
                                (maybe Bytes.Decode.Util.name)

                    3 ->
                        Decode.succeed (Ok Ast.Canonical.TUnit)

                    4 ->
                        Decode.succeed (Result.map3 Ast.Canonical.TTuple)
                            |> keeper type_
                            |> keeper type_
                            |> keeper
                                (maybe type_)

                    5 ->
                        Decode.succeed (Result.map4 Ast.Canonical.TAlias)
                            |> keeper Bytes.Decode.ElmFile.Module.name
                            |> keeper Bytes.Decode.Util.name
                            |> keeper
                                (Bytes.Decode.Util.list
                                    (Decode.succeed (Result.map2 Tuple.pair)
                                        |> keeper Bytes.Decode.Util.name
                                        |> keeper type_
                                    )
                                )
                            |> keeper aliasType

                    6 ->
                        Decode.succeed (Result.map3 Ast.Canonical.TType)
                            |> keeper Bytes.Decode.ElmFile.Module.name
                            |> keeper Bytes.Decode.Util.name
                            |> keeper (Bytes.Decode.Util.list type_)

                    n ->
                        Decode.succeed (Result.map3 Ast.Canonical.TType)
                            |> keeper Bytes.Decode.ElmFile.Module.name
                            |> keeper Bytes.Decode.Util.name
                            |> keeper (Bytes.Decode.Util.listLength (n - 7) type_)
            )



-- Util functions


keeper : Decoder a -> Decoder (a -> b) -> Decoder b
keeper parseArg parseFunc =
    Decode.map2 (<|) parseFunc parseArg


dictToSet : Dict comparable v -> Set comparable
dictToSet =
    Dict.toList
        >> List.map Tuple.first
        >> Set.fromList


maybe : Decoder64 a -> Decoder64 (Maybe a)
maybe d =
    Decode.unsignedInt8
        |> Decode.andThen
            (\id ->
                case id of
                    0 ->
                        Decode.succeed Nothing

                    1 ->
                        Decode.map Just d

                    _ ->
                        Decode.fail
            )
        |> Decode.map
            (Maybe.map (Result.map Just)
                >> Maybe.withDefault (Ok Nothing)
            )
