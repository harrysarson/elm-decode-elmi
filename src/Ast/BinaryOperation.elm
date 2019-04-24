module Ast.BinaryOperation exposing (Associativity(..), Precedence(..))

{-|

@docs Associativity, Precedence

-}


{-| Precedence of a binary operation.

Precedence determines the order of operatiations. For example, in `5 * 7 -2`
the `*` operation has higher precedence and so `5 * 7` is evaluated first.

-}
type Precedence
    = Precedence Int


{-| Associativity of a binary operation.

Precedence makes sense of repeated use of operators without parentheses. For
example in `x * y * z`, `x * y` is evaluated first as `*` is left associative.

-}
type Associativity
    = Left
    | None
    | Right
