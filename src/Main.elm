port module Main exposing (..)

import Dict exposing (Dict)
import FindModulesToParse exposing (getModulesToParse)
import FirstPass exposing (parseModule)
import Json.Decode
import PackageInfo exposing (PackageInfo)
import Process
import ReadSourceFilesProgress
import Task
import Time exposing (Time)


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
    , readSourceFilesProgress : ReadSourceFilesProgress.Model
    , sourceFiles : Dict ModuleName SourceCode
    }


type Msg
    = Stop
    | Abort
    | ReadSourceFilesProgressMsg ReadSourceFilesProgress.Msg


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
                    modulesToParse : List String
                    modulesToParse =
                        viewModuleContents
                            |> parseModule
                            |> getModulesToParse

                    -- path = TODO get full path, dir, name
                    _ =
                        Debug.log "files to parse" modulesToParse

                    srcDirs =
                        packageInfo.sourceDirectories

                    -- paths =
                    --     modulesToParse |> List.map qualifiedNameToPath
                    ( readSourceFilesProgress, readSourceFilesProgressCmd ) =
                        ReadSourceFilesProgress.init modulesToParse srcDirs

                    -- _ =
                    --     Debug.log "paths" paths
                in
                    { packageInfo = packageInfo
                    , readSourceFilesProgress = readSourceFilesProgress
                    , sourceFiles = Dict.empty
                    }
                        ! [ readSourceFilesProgressCmd
                                |> Cmd.map ReadSourceFilesProgressMsg
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
    case msg of
        Stop ->
            model ! [ exitApp 0 ]

        Abort ->
            model ! [ exitApp -1 ]

        ReadSourceFilesProgressMsg rsfpMsg ->
            { model
                | readSourceFilesProgress =
                    ReadSourceFilesProgress.update
                        rsfpMsg
                        model.readSourceFilesProgress
            }
                ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ externalStop <| always Abort
        , ReadSourceFilesProgress.subscriptions
            |> Sub.map ReadSourceFilesProgressMsg
        ]



-- UTILITIES


delayMsg : Time -> Msg -> Cmd Msg
delayMsg time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)
