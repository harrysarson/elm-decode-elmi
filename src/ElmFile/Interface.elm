module ElmFile.Interface exposing (..)

import Ast.Canonical
import Dict exposing (Dict)
import Ast.BinaryOperation
import ElmFile.Module

{-| A list of interfaces defined by the modueles in a package.
-}
type Interfaces
    = Interfaces (List (ElmFile.Module.Name, Interface))

{-| The interface defined by a module.
-}
type Interface
    = Interface
        { types_ : Dict String Ast.Canonical.Annotation
        , unions : Dict String Union
        , aliases : Dict String Alias
        , binaryOperations: Dict String BinaryOperation
        }


{-| A custom type in an elm file.
-}
type Union
    = OpenUnion Ast.Canonical.Union
    | ClosedUnion Ast.Canonical.Union
    | PrivateUnion Ast.Canonical.Union


{-| A type alias in an elm file.
-}
type Alias
    = PublicAlias Ast.Canonical.Alias
    | PrivateAlias Ast.Canonical.Alias



{-| A binary option in an elm file.

Note: these will only show up in the interface exported by modules from
the blessed elm/* packages.

-}
type BinaryOperation =
  BinaryOperation
    { name : String
    , annotation : Ast.Canonical.Annotation
    , associativity : Ast.BinaryOperation.Associativity
    , precedence : Ast.BinaryOperation.Precedence
    }
