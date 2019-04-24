module ElmFile.Module exposing (Name(..))

{-|

@docs Name

-}

import ElmFile.Package


{-| The name of an elm module, including the package it is from.

Note: I am not sure what this looks like for the name of a module from an
elm application. I suspect that `package` and `application` can be used
interchangably here.

-}
type Name
    = Name
        { package : ElmFile.Package.Name
        , module_ : String
        }
