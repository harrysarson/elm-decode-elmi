module Bytes.Decode.Ast.Canonical exposing (alias, aliasType, annotation, fieldtype, type_, union, unionCtor)

{-|

@docs alias, aliasType, annotation, fieldtype, type_, union, unionCtor

-}

import Ast.Canonical
import Bytes
import Bytes.Decode as Decode exposing (Decoder)
import Bytes.Decode.ElmFile.Module
import Bytes.Decode.Util
import Bytes.Encode as Encode exposing (Encoder)
import Dict exposing (Dict)
import ElmFile.Interface exposing (Interface)
import ElmFile.Module
import Maybe.Extra
import Result.Extra
import Set exposing (Set)


{-| Decoder for an elm custom type.
-}
union : (Int -> Int -> any) -> Decoder Ast.Canonical.Union
union cb =
    Decode.succeed
        (\vars alts ->
            Ast.Canonical.Union
                { vars = vars
                , alts = alts
                }
        )
        |> keeper (Bytes.Decode.Util.list (Bytes.Decode.Util.name cb) cb)
        |> keeper (Bytes.Decode.Util.list (unionCtor cb) cb)
        |> ignore (Bytes.Decode.Util.uint64 cb)
        |> ignore unionCtorOpts


{-| Decoder for an elm type alias.
-}
alias : (Int -> Int -> any) -> Decoder Ast.Canonical.Alias
alias cb =
    Decode.succeed Ast.Canonical.Alias
        |> keeper (Bytes.Decode.Util.list (Bytes.Decode.Util.name cb) cb)
        |> keeper (type_ cb)


{-| Decoder for the constructor of an elm custom type alternative.
-}
unionCtor : (Int -> Int -> any) -> Decoder Ast.Canonical.UnionCtor
unionCtor cb =
    Decode.succeed Ast.Canonical.UnionCtor
        |> keeper (Bytes.Decode.Util.name cb)
        |> keeper (Bytes.Decode.Util.uint64 cb)
        |> keeper (Bytes.Decode.Util.uint64 cb)
        |> keeper (Bytes.Decode.Util.list (type_ cb) cb)


unionCtorOpts : Decoder ()
unionCtorOpts =
    Decode.unsignedInt8
        |> Decode.andThen
            (\id ->
                case id of
                    0 ->
                        Decode.succeed ()

                    -- Ast.Canonical.CtorOptNormal
                    1 ->
                        Decode.succeed ()

                    -- Ast.Canonical.CtorOptEnum
                    2 ->
                        Decode.succeed ()

                    -- Ast.Canonical.CtorOptUnbox
                    _ ->
                        Decode.fail
            )


{-| Decoder for an elm type annotation.
-}
annotation : (Int -> Int -> any) -> Decoder Ast.Canonical.Annotation
annotation cb =
    Decode.map2 Ast.Canonical.Annotation
        (Bytes.Decode.Util.decodeDict
            (Bytes.Decode.Util.name cb)
            (Decode.succeed ())
            cb
            |> Decode.map dictToSet
        )
        (type_ cb)


{-| Decoder whether an elm type alias is holey or not.
-}
aliasType : (Int -> Int -> any) -> Decoder Ast.Canonical.AliasType
aliasType cb =
    Decode.unsignedInt8
        |> Decode.andThen
            (\id ->
                case id of
                    0 ->
                        Decode.succeed Ast.Canonical.Holey
                            |> keeper (type_ cb)

                    1 ->
                        Decode.succeed Ast.Canonical.Filled
                            |> keeper (type_ cb)

                    _ ->
                        Decode.fail
            )


{-| Decoder the index and type of a field in an elm record.
-}
fieldtype : (Int -> Int -> any) -> Decoder Ast.Canonical.FieldType
fieldtype cb =
    Decode.succeed Ast.Canonical.FieldType
        |> keeper (Decode.unsignedInt16 Bytes.BE)
        |> keeper (type_ cb)


{-| Decoder an elm type.
-}
type_ : (Int -> Int -> any) -> Decoder Ast.Canonical.Type
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
                        Decode.succeed Ast.Canonical.TUnit

                    4 ->
                        Decode.succeed Ast.Canonical.TTuple
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
                        Decode.succeed Ast.Canonical.TType
                            |> keeper (Bytes.Decode.ElmFile.Module.name cb)
                            |> keeper (Bytes.Decode.Util.name cb)
                            |> keeper (Bytes.Decode.Util.list (type_ cb) cb)

                    n ->
                        Decode.succeed Ast.Canonical.TType
                            |> keeper (Bytes.Decode.ElmFile.Module.name cb)
                            |> keeper (Bytes.Decode.Util.name cb)
                            |> keeper (Bytes.Decode.Util.listLength (n - 7) (type_ cb))
            )



-- Util functions


keeper : Decoder a -> Decoder (a -> b) -> Decoder b
keeper parseArg parseFunc =
    Decode.map2 (<|) parseFunc parseArg


ignore : Decoder ignore -> Decoder keep -> Decoder keep
ignore parseIgnorer parseKeeper =
    Decode.map2 always parseKeeper parseIgnorer


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
