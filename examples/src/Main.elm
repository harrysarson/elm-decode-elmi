port module Main exposing ( main)

import Ast.Canonical
import Base64
import Bytes.Decode
import Bytes.Encode
import Bytes.Decode.ElmFile.Interface
import ElmFile.Interface
import ElmFile.Module
import ElmFile.Package
import Dict
import Platform
import Debug

port scanElmi : (String -> msg) -> Sub msg

port scanIfacesDat : (String -> msg) -> Sub msg

type alias Flags =
    { elmi : String
    }

type Error
    = DecodingError
    | EncodingBase64Failed

type Msg
    = ScanElmi String
    | ScanIfacesDat String

main : Platform.Program () () Msg
main =
    Platform.worker
        { init = always  ( (), Cmd.none )
        , update = update
        , subscriptions = always <| Sub.batch
            [ scanElmi ScanElmi
            , scanIfacesDat ScanIfacesDat
            ]
        }

update : Msg -> () -> ((), Cmd never)
update msg () =
    case msg of
        ScanElmi elmiStr ->
            let
                elmi =
                    Base64.toBytes elmiStr
                        |> Result.fromMaybe (EncodingBase64Failed)

                interface =
                    elmi
                        |> Result.andThen
                            (Bytes.Decode.decode (Bytes.Decode.ElmFile.Interface.interface logger)
                                >> Result.fromMaybe (DecodingError))

                _ =
                    interface
                        |> Result.map (\(ElmFile.Interface.Interface i) -> i)
                        |> Result.map .binaryOperations
                        |> Debug.log "binaryops"

                tests =
                    interface
                        |> Result.map (\(ElmFile.Interface.Interface i) -> i)
                        |> Result.map .types_
                        |> Result.map (
                                Dict.foldl
                                    (\name annotation ts ->
                                        let
                                            (Ast.Canonical.Annotation _ t) =
                                                annotation
                                        in
                                            case t of
                                                Ast.Canonical.TAlias (ElmFile.Module.Name moduleName) "Test" _ _ ->
                                                    let (ElmFile.Package.Name packageName) = moduleName.package in
                                                    if packageName.author == "elm-explorations" &&
                                                        packageName.project == "test" &&
                                                        moduleName.module_ == "Test" then
                                                        name :: ts
                                                    else
                                                        ts
                                                _ ->
                                                    ts
                                    )
                                    []
                                    )


                _ =
                    tests
                        |> Result.map (\okTest -> Debug.log "Tests exposed" (String.join ", " okTest))
            in
                ((), Cmd.none)

        ScanIfacesDat ifacesStr ->
            let
                elmi =
                    Base64.toBytes ifacesStr
                        |> Result.fromMaybe (EncodingBase64Failed)


                interfaces =
                    elmi
                        |> Result.andThen
                            ((Bytes.Decode.decode (Bytes.Decode.ElmFile.Interface.interfaces logger))
                                >> Result.fromMaybe DecodingError
                                )

                _ =
                    interfaces
                        |> Result.map (\(ElmFile.Interface.Interfaces is) -> is)
                        |> Result.map (List.map (\(name, i) ->
                            let
                                (ElmFile.Interface.Interface int) = i
                            in
                            Debug.log (Debug.toString name ++ " binaryops") int.binaryOperations
                        ))
                        |> Result.mapError ( Debug.log "Decoding error:")
            in
                ((), Cmd.none)

logger : Int -> Int -> never
logger upper lower =
    Debug.todo ("64 bit number found in binary (" ++ String.fromInt upper ++ " << 32 + " ++ String.fromInt lower ++ ")")
