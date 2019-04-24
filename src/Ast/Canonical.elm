module Ast.Canonical exposing (Alias(..), AliasType(..), Annotation(..), FieldType(..), Type(..), Union(..), UnionCtor(..))

{-|

@docs Alias, AliasType, Annotation, FieldType, Type, Union, UnionCtor

-}

import Dict exposing (Dict)
import ElmFile.Module
import Set exposing (Set)


{-| A type definition or alias in elm.
-}
type Type
    = TLambda Type Type
    | TVar String
    | TType ElmFile.Module.Name String (List Type)
    | TRecord (Dict String FieldType) (Maybe String)
    | TUnit
    | TTuple Type Type (Maybe Type)
    | TAlias ElmFile.Module.Name String (List ( String, Type )) AliasType


{-| A field in an elm record with an index and a type.
-}
type FieldType
    = FieldType Int Type


{-| An type alias can either be holey or filled.

I believe that holey type aliases are those with generic parameters.

-}
type AliasType
    = Holey Type
    | Filled Type


{-| An elm type annotation.

The set of strings are the free type variables that the annotation contains.

-}
type Annotation
    = Annotation (Set String) Type


{-| An elm custom type.

This record stores the type variables in `vars` and the "options that you would
write a case statement for" in `alts`.

Note: the binary data also includes the number of alternatives and some
optimation information - this is all currently discarded.

-}
type Union
    = Union
        { vars : List String
        , alts : List UnionCtor
        }


{-| A type alias.
-}
type Alias
    = Alias (List String) Type


{-| A contructor for a custom type alternative.

TODO: document the meaning of each arg.

Note: some of these ints are redundant.

-}
type UnionCtor
    = UnionCtor String Int Int (List Type)
