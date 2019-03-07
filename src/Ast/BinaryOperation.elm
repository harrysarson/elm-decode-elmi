module Ast.BinaryOperation exposing (..)


type Precedence
    = Precedence Int


type Associativity
  = Left
  | None
  | Right
