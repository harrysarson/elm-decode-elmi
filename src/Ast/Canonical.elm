module Ast.Canonical exposing (Alias(..), AliasType(..), Annotation(..), FieldType(..), Type(..), Union(..), UnionCtor(..), UnionCtorOpts(..))

import ElmFile.Module
import Dict exposing (Dict)
import Set exposing (Set)


type Type
    = TLambda Type Type
    | TVar String
    | TType ElmFile.Module.Name String (List Type)
    | TRecord (Dict String FieldType) (Maybe String)
    | TUnit
    | TTuple Type Type (Maybe Type)
    | TAlias ElmFile.Module.Name String (List ( String, Type )) AliasType


type FieldType
    = FieldType Int Type


type AliasType
    = Holey Type
    | Filled Type


type Annotation
    = Annotation (Set String) Type


type Union
    = Union
        { vars : List String
        , alts : List UnionCtor
        , numAlts : Int
        , opts : UnionCtorOpts
        }


type Alias
    = Alias (List String) Type


{-| TODO: are some of these (ints?) redundant?
-}
type UnionCtor
    = UnionCtor String Int Int (List Type)


type UnionCtorOpts
    = CtorOptNormal
    | CtorOptEnum
    | CtorOptUnbox
