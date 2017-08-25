port module Main exposing (..)

import Dict exposing (Dict)
import DetermineWhichModulesToLoad
import FirstPass exposing (parseModule)
import Json.Decode
import PackageInfo exposing (PackageInfo)
import Process
import ReadSourceFiles
import Task
import Time exposing (Time)
import DeterminePackageLocations


{- REMOVE WHEN COMPILER BUG IS FIXED -}

import Json.Decode


port exitApp : Float -> Cmd msg


port externalStop : (() -> msg) -> Sub msg


type alias Flags =
    { elmPackageContents : String
    , subjectSourceCode : String
    }


main : Program Flags Model Msg
main =
    Platform.programWithFlags
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


type alias SourceCode =
    String


type alias ModuleName =
    String


type alias Model =
    { packageInfo : PackageInfo
    , readSourceFiles : ReadSourceFiles.Model
    , determinePackageLocations : DeterminePackageLocations.Model
    , packageDirs : Maybe (List String)
    , sourceFiles : Dict ModuleName SourceCode
    , subjectSourceCode : String
    }


type Msg
    = Stop
    | Abort
    | ReadSourceFilesMsg ReadSourceFiles.Msg
    | DeterminePackageLocationsMsg DeterminePackageLocations.Msg


init : Flags -> ( Model, Cmd Msg )
init { elmPackageContents, subjectSourceCode } =
    let
        packageInfoResult : Result String PackageInfo
        packageInfoResult =
            Json.Decode.decodeString PackageInfo.decoder elmPackageContents
    in
        case packageInfoResult of
            Ok packageInfo ->
                let
                    ( determinePackageLocationsModel, determinePackageLocationsCmd ) =
                        DeterminePackageLocations.init packageInfo
                in
                    { subjectSourceCode = subjectSourceCode
                    , packageInfo = packageInfo
                    , readSourceFiles = ReadSourceFiles.init
                    , sourceFiles = Dict.empty
                    , determinePackageLocations = determinePackageLocationsModel
                    , packageDirs = Nothing
                    }
                        ! [ determinePackageLocationsCmd
                                |> Cmd.map DeterminePackageLocationsMsg
                          ]

            Err err ->
                let
                    err2 =
                        "Invalid elm-package.json.\n\n " ++ err
                in
                    Debug.crash err2


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    -- let
    --     _ =
    --         Debug.log "\n\nmodel.readSourceFiles\n" model.readSourceFiles
    -- in
    case msg of
        Stop ->
            model ! [ exitApp 0 ]

        Abort ->
            model ! [ exitApp -1 ]

        ReadSourceFilesMsg rsfpMsg ->
            let
                -- _ =
                --     Debug.log "ReadSourceFilesMsg" True
                ( readSourceFiles, readSourceFilesCmd ) =
                    ReadSourceFiles.update
                        rsfpMsg
                        model.readSourceFiles

                newModel =
                    { model | readSourceFiles = readSourceFiles }

                -- _ =
                --     Debug.log "\n\nnewModel\n" newModel
            in
                newModel ! [ readSourceFilesCmd |> Cmd.map ReadSourceFilesMsg ]

        DeterminePackageLocationsMsg dplMsg ->
            let
                ( determinePackageLocations, maybePackageLocations ) =
                    DeterminePackageLocations.update dplMsg model.determinePackageLocations

                modulesToLoad =
                    model.subjectSourceCode
                        |> parseModule
                        |> DetermineWhichModulesToLoad.doIt
                        |> .modulesToLoad

                ( readSourceFiles, readSourceFilesCmd ) =
                    case maybePackageLocations of
                        Just packageLocations ->
                            ReadSourceFiles.reallyInit
                                { dirNames = packageLocations ++ model.packageInfo.sourceDirectories
                                , moduleNames = modulesToLoad
                                }
                                model.readSourceFiles

                        Nothing ->
                            ( model.readSourceFiles, Cmd.none )
            in
                { model
                    | determinePackageLocations = determinePackageLocations
                    , packageDirs = maybePackageLocations
                    , readSourceFiles = readSourceFiles
                }
                    ! [ readSourceFilesCmd |> Cmd.map ReadSourceFilesMsg ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ externalStop <| always Abort
        , ReadSourceFiles.subscriptions
            |> Sub.map ReadSourceFilesMsg
        , DeterminePackageLocations.subscriptions
            |> Sub.map DeterminePackageLocationsMsg
        ]



-- UTILITIES


delayMsg : Time -> Msg -> Cmd Msg
delayMsg time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)
