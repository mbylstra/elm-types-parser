port module Main exposing (..)

import Dict exposing (Dict)
import FindModulesToParse exposing (getModulesToParse)
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
    , viewModuleContents : String
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
    , readSourceFilesProgress : ReadSourceFiles.Model
    , determinePackageLocations : DeterminePackageLocations.Model
    , sourceFiles : Dict ModuleName SourceCode
    }


type Msg
    = Stop
    | Abort
    | ReadSourceFilesMsg ReadSourceFiles.Msg
    | DeterminePackageLocationsMsg DeterminePackageLocations.Msg


init : Flags -> ( Model, Cmd Msg )
init { elmPackageContents, viewModuleContents } =
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

                    modulesToParse : List String
                    modulesToParse =
                        viewModuleContents
                            |> parseModule
                            |> getModulesToParse

                    -- path = TODO get full path, dir, name
                    -- _ =
                    --     Debug.log "files to parse" modulesToParse
                    srcDirs =
                        packageInfo.sourceDirectories

                    -- paths =
                    --     modulesToParse |> List.map qualifiedNameToPath
                    ( readSourceFilesProgress, readSourceFilesProgressCmd ) =
                        ReadSourceFiles.init modulesToParse srcDirs

                    -- _ =
                    --     Debug.log "paths" paths
                in
                    { packageInfo = packageInfo
                    , readSourceFilesProgress = readSourceFilesProgress
                    , sourceFiles = Dict.empty
                    , determinePackageLocations = determinePackageLocationsModel
                    }
                        -- ! [ readSourceFilesProgressCmd
                        --         |> Cmd.map ReadSourceFilesMsg
                        ! [ determinePackageLocationsCmd
                                |> Cmd.map DeterminePackageLocationsMsg
                          ]

            -- we need to figure out what inital commands we need based on the current state (same as after an update)
            Err err ->
                let
                    err2 =
                        "Invalid elm-package.json.\n\n " ++ err
                in
                    Debug.crash err2


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        Stop ->
            model ! [ exitApp 0 ]

        Abort ->
            model ! [ exitApp -1 ]

        ReadSourceFilesMsg rsfpMsg ->
            { model
                | readSourceFilesProgress =
                    ReadSourceFiles.update
                        rsfpMsg
                        model.readSourceFilesProgress
            }
                ! []

        DeterminePackageLocationsMsg dplMsg ->
            let
                _ =
                    Debug.log "dplsMsg" dplMsg
            in
                { model | determinePackageLocations = DeterminePackageLocations.update dplMsg model.determinePackageLocations } ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ externalStop <| always Abort

        -- , ReadSourceFiles.subscriptions
        --     |> Sub.map ReadSourceFilesMsg
        -- , DeterminePackageLocation.subscriptions
        --     |> Sub.map DeterminePackageLocationMsg
        ]



-- UTILITIES


delayMsg : Time -> Msg -> Cmd Msg
delayMsg time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)
