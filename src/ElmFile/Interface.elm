module ElmFile.Interface exposing (Alias(..), Interface(..), Union(..))

import Ast.Canonical exposing (Annotation)
import Dict exposing (Dict)


type Interface
    = Interface
        { types_ : Dict String Annotation
        , unions : Dict String Union
        , aliases : Dict String Alias
        }


type Union
    = OpenUnion Ast.Canonical.Union
    | ClosedUnion Ast.Canonical.Union
    | PrivateUnion Ast.Canonical.Union


type Alias
    = PublicAlias Ast.Canonical.Alias
    | PrivateAlias Ast.Canonical.Alias
