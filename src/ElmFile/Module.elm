module ElmFile.Module exposing (Name(..))

import ElmFile.Package


type Name
    = Name
        { package : ElmFile.Package.Name
        , module_ : String
        }
