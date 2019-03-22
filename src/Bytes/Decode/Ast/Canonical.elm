module Bytes.Decode.Ast.Canonical exposing (aliasType, alias_, annotation, fieldtype, type_, union, unionCtor, unionCtorOpts)

import Ast.Canonical
import ElmFile.Interface exposing (Interface)
import ElmFile.Module
import Bytes
import Bytes.Decode as Decode exposing (Decoder)
import Bytes.Encode as Encode exposing (Encoder)
import Bytes.Decode.ElmFile.Module
import Bytes.Decode.Util
import Dict exposing (Dict)
import Maybe.Extra
import Result.Extra
import Set exposing (Set)


union : (Int -> Int -> any) -> Decoder Ast.Canonical.Union
union cb =
    Decode.succeed
         (\vars alts numAlts opts ->
                Ast.Canonical.Union
                    { vars = vars
                    , alts = alts
                    , numAlts = numAlts
                    , opts = opts
                    }
            )
        |> keeper (Bytes.Decode.Util.list (Bytes.Decode.Util.name cb) cb)
        |> keeper (Bytes.Decode.Util.list (unionCtor cb) cb)
        |> keeper (Bytes.Decode.Util.uint64 cb)
        |> keeper unionCtorOpts


alias_ : (Int -> Int -> any) -> Decoder Ast.Canonical.Alias
alias_ cb =
    Decode.succeed (Ast.Canonical.Alias)
        |> keeper (Bytes.Decode.Util.list (Bytes.Decode.Util.name cb) cb)
        |> keeper (type_ cb)


unionCtor : (Int -> Int -> any) -> Decoder Ast.Canonical.UnionCtor
unionCtor cb =
    Decode.succeed (Ast.Canonical.UnionCtor)
        |> keeper (Bytes.Decode.Util.name cb)
        |> keeper (Bytes.Decode.Util.uint64 cb)
        |> keeper (Bytes.Decode.Util.uint64 cb)
        |> keeper (Bytes.Decode.Util.list (type_ cb) cb)


unionCtorOpts : Decoder Ast.Canonical.UnionCtorOpts
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


annotation : (Int -> Int -> any) -> Decoder Ast.Canonical.Annotation
annotation cb =
    Decode.map2 (Ast.Canonical.Annotation)
        (Bytes.Decode.Util.decodeDict
            (Bytes.Decode.Util.name cb)
            (Decode.succeed ())
            cb
            |> Decode.map dictToSet
        )
        (type_ cb)


aliasType : (Int -> Int -> any) -> Decoder Ast.Canonical.AliasType
aliasType cb =
    Decode.unsignedInt8
        |> Decode.andThen
            (\id ->
                case id of
                    0 ->
                        Decode.succeed (Ast.Canonical.Holey)
                            |> keeper (type_ cb)

                    1 ->
                        Decode.succeed (Ast.Canonical.Filled)
                            |> keeper (type_ cb)

                    _ ->
                        Decode.fail
            )


fieldtype : (Int -> Int -> any) -> Decoder Ast.Canonical.FieldType
fieldtype cb =
    Decode.succeed Ast.Canonical.FieldType
        |> keeper (Decode.unsignedInt16 Bytes.BE)
        |> keeper (type_ cb)


type_ : (Int -> Int -> any) ->  Decoder Ast.Canonical.Type
type_ cb =
    Decode.unsignedInt8
        |> Decode.andThen
            (\id ->
                case id of
                    0 ->
                        Decode.succeed Ast.Canonical.TLambda
                            |> keeper (type_ cb)
                            |> keeper (type_ cb)

                    1 ->
                        Decode.succeed Ast.Canonical.TVar
                            |> keeper (Bytes.Decode.Util.name cb)

                    2 ->
                        Decode.succeed Ast.Canonical.TRecord
                            |> keeper (Bytes.Decode.Util.decodeDict (Bytes.Decode.Util.name cb) (fieldtype cb) cb)
                            |> keeper
                                (maybe (Bytes.Decode.Util.name cb))

                    3 ->
                        Decode.succeed  Ast.Canonical.TUnit

                    4 ->
                        Decode.succeed (Ast.Canonical.TTuple)
                            |> keeper (type_ cb)
                            |> keeper (type_ cb)
                            |> keeper
                                (maybe (type_ cb))

                    5 ->
                        Decode.succeed Ast.Canonical.TAlias
                            |> keeper (Bytes.Decode.ElmFile.Module.name cb)
                            |> keeper (Bytes.Decode.Util.name cb)
                            |> keeper
                                (Bytes.Decode.Util.list
                                    (Decode.succeed Tuple.pair
                                        |> keeper (Bytes.Decode.Util.name cb)
                                        |> keeper (type_ cb)
                                    )
                                    cb
                                )
                            |> keeper (aliasType cb)

                    6 ->
                        Decode.succeed (Ast.Canonical.TType)
                            |> keeper (Bytes.Decode.ElmFile.Module.name cb)
                            |> keeper (Bytes.Decode.Util.name cb)
                            |> keeper (Bytes.Decode.Util.list (type_ cb) cb)

                    n ->
                        Decode.succeed (Ast.Canonical.TType)
                            |> keeper (Bytes.Decode.ElmFile.Module.name cb)
                            |> keeper (Bytes.Decode.Util.name cb)
                            |> keeper (Bytes.Decode.Util.listLength (n - 7) (type_ cb))
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


maybe : Decoder a -> Decoder (Maybe a)
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
