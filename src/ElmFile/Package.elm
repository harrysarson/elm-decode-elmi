module ElmFile.Package exposing (Name(..))

{-|

@docs Name

-}


{-| The name of an elm package.

Note: This might also be the name of an elm application - in which case what
would the author be?

-}
type Name
    = Name
        { author : String
        , project : String
        }
