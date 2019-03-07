port module Main exposing ( main)

import Ast.Canonical
import Base64
import Bytes.Decode
import Bytes.Encode
import Decode.ElmFile.Interface
import ElmFile.Interface
import ElmFile.Module
import ElmFile.Package
import Dict
import Platform
import Decode.Util.Decode64

port scanElmi : (String -> msg) -> Sub msg

port scanIfacesDat : (String -> msg) -> Sub msg

type alias Flags =
    { elmi : String
    }

type Error
    = DecodingError
    | EncodingBase64Failed
    | FormatError Decode.Util.Decode64.Error

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
                            (Bytes.Decode.decode Decode.ElmFile.Interface.interface
                                >> Maybe.map (Result.mapError FormatError)
                                >> Maybe.withDefault (Err DecodingError))

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
                            (Bytes.Decode.decode Decode.ElmFile.Interface.interfaces
                                >> Maybe.map (Result.mapError FormatError)
                                >> Maybe.withDefault (Err DecodingError))

                _ =
                    interfaces
                        |> Result.map (\(ElmFile.Interface.Interfaces is) -> is)
                        |> Result.map (List.map (\(name, i) ->
                            let
                                (ElmFile.Interface.Interface int) = i
                            in
                            Debug.log (Debug.toString name ++ " binaryops") int.binaryOperations
                        ))
            in
                ((), Cmd.none)
