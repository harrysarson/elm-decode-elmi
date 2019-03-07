module ElmFile.Interface exposing (..)

import Ast.Canonical
import Dict exposing (Dict)
import Ast.BinaryOperation
import ElmFile.Module

type Interfaces
    = Interfaces (List (ElmFile.Module.Name, Interface))

type Interface
    = Interface
        { types_ : Dict String Ast.Canonical.Annotation
        , unions : Dict String Union
        , aliases : Dict String Alias
        , binaryOperations: Dict String BinaryOperation
        }


type Union
    = OpenUnion Ast.Canonical.Union
    | ClosedUnion Ast.Canonical.Union
    | PrivateUnion Ast.Canonical.Union


type Alias
    = PublicAlias Ast.Canonical.Alias
    | PrivateAlias Ast.Canonical.Alias



type BinaryOperation =
  BinaryOperation
    { name : String
    , annotation : Ast.Canonical.Annotation
    , associativity : Ast.BinaryOperation.Associativity
    , precedence : Ast.BinaryOperation.Precedence
    }
