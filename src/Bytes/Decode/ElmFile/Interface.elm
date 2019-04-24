module Bytes.Decode.ElmFile.Interface exposing (alias, binaryOperation, interface, interfaces, union)

{-|

@docs alias, binaryOperation, interface, interfaces, union

-}

import Ast.BinaryOperation
import Ast.Canonical
import Bytes
import Bytes.Decode as Decode exposing (Decoder)
import Bytes.Decode.Ast.BinaryOperation
import Bytes.Decode.Ast.Canonical
import Bytes.Decode.ElmFile.Module
import Bytes.Decode.Util
import Bytes.Encode as Encode exposing (Encoder)
import Dict exposing (Dict)
import ElmFile.Interface exposing (Interface, Interfaces)
import ElmFile.Module
import Set exposing (Set)


{-| Decoder for the Interfaces type.

This function will decode the binary of an `ifaces.dat` file found in the elm
home directrory.

-}
interfaces : (Int -> Int -> any) -> Decoder Interfaces
interfaces cb =
    Decode.map
        ElmFile.Interface.Interfaces
        (Bytes.Decode.Util.list
            (Decode.map2
                Tuple.pair
                (Bytes.Decode.ElmFile.Module.name cb)
                (interface cb)
            )
            cb
        )


{-| Decoder for the Interface type.

This function will decode the binary of an `.elmi` file found in `elm-stuff`
directories.

-}
interface : (Int -> Int -> any) -> Decoder Interface
interface cb =
    Decode.map4
        (\d u a b ->
            ElmFile.Interface.Interface
                { types_ = d
                , unions = u
                , aliases = a
                , binaryOperations = b
                }
        )
        (Bytes.Decode.Util.decodeDict
            (Bytes.Decode.Util.name cb)
            (Bytes.Decode.Ast.Canonical.annotation cb)
            cb
        )
        (Bytes.Decode.Util.decodeDict
            (Bytes.Decode.Util.name cb)
            (union cb)
            cb
        )
        (Bytes.Decode.Util.decodeDict
            (Bytes.Decode.Util.name cb)
            (alias cb)
            cb
        )
        (Bytes.Decode.Util.decodeDict
            (Bytes.Decode.Util.name cb)
            (binaryOperation cb)
            cb
        )


{-| Decoder for a custom type.
-}
union : (Int -> Int -> any) -> Decoder ElmFile.Interface.Union
union cb =
    Decode.unsignedInt8
        |> Decode.andThen
            (\id ->
                case id of
                    0 ->
                        Bytes.Decode.Ast.Canonical.union cb |> Decode.map ElmFile.Interface.OpenUnion

                    1 ->
                        Bytes.Decode.Ast.Canonical.union cb |> Decode.map ElmFile.Interface.ClosedUnion

                    2 ->
                        Bytes.Decode.Ast.Canonical.union cb |> Decode.map ElmFile.Interface.PrivateUnion

                    _ ->
                        Decode.fail
            )



{-| Decoder for a type alias.
-}
alias : (Int -> Int -> any) -> Decoder ElmFile.Interface.Alias
alias cb =
    Decode.unsignedInt8
        |> Decode.andThen
            (\id ->
                case id of
                    0 ->
                        Bytes.Decode.Ast.Canonical.alias cb |> Decode.map ElmFile.Interface.PublicAlias

                    1 ->
                        Bytes.Decode.Ast.Canonical.alias cb |> Decode.map ElmFile.Interface.PrivateAlias

                    _ ->
                        Decode.fail
            )


{-| Decoder for a binary operation.
-}
binaryOperation : (Int -> Int -> any) -> Decoder ElmFile.Interface.BinaryOperation
binaryOperation cb =
    Decode.map4
        (\n an ass p ->
            ElmFile.Interface.BinaryOperation
                { name = n
                , annotation = an
                , associativity = ass
                , precedence = p
                }
        )
        (Bytes.Decode.Util.name cb)
        (Bytes.Decode.Ast.Canonical.annotation cb)
        Bytes.Decode.Ast.BinaryOperation.associativity
        (Bytes.Decode.Ast.BinaryOperation.precedence cb)
