port module Main exposing (..)

-- import Dict exposing (Dict)

import ModuleInfo
import Json.Decode
import PackageInfo exposing (PackageInfo)
import Process
import ReadSourceFiles
import Task
import Time exposing (Time)
import DeterminePackageLocations
import Types exposing (ModuleInfo)


{- REMOVE WHEN COMPILER BUG IS FIXED -}

import Json.Decode


port exitApp : Float -> Cmd msg


port externalStop : (() -> msg) -> Sub msg


type alias Flags =
    { elmPackageContents : String
    , subjectSourceCode : String
    , exactDependenciesContents : String
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
    { sourceDirectories : List String
    , readSourceFilesModel : ReadSourceFiles.Model
    , packageDirs : List String

    -- , sourceFiles : Dict ModuleName SourceCode
    , subjectSourceCode : String
    , subjectModuleInfo : ModuleInfo
    }


type Msg
    = Stop
    | Abort
    | ReadSourceFilesMsg ReadSourceFiles.Msg


init : Flags -> ( Model, Cmd Msg )
init { elmPackageContents, subjectSourceCode, exactDependenciesContents } =
    let
        packageInfoResult : Result String PackageInfo
        packageInfoResult =
            Json.Decode.decodeString PackageInfo.decoder elmPackageContents
    in
        case packageInfoResult of
            Ok packageInfo ->
                let
                    packageDirs =
                        DeterminePackageLocations.doIt exactDependenciesContents

                    sourceDirectories =
                        packageInfo.sourceDirectories

                    subjectModuleInfo =
                        subjectSourceCode
                            |> ModuleInfo.getModuleInfo

                    -- we need to store this in the model, so that once we've read the source files,
                    -- we can match it
                    modulesToLoad =
                        subjectModuleInfo
                            |> ModuleInfo.getModulesToLoad

                    ( readSourceFilesModel, readSourceFilesCmd ) =
                        ReadSourceFiles.init
                            { dirNames = packageDirs ++ sourceDirectories
                            , moduleNames = modulesToLoad
                            }
                in
                    { subjectSourceCode = subjectSourceCode
                    , sourceDirectories = sourceDirectories
                    , readSourceFilesModel = readSourceFilesModel
                    , packageDirs = packageDirs
                    , subjectModuleInfo = subjectModuleInfo
                    }
                        ! [ readSourceFilesCmd |> Cmd.map ReadSourceFilesMsg ]

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

        ReadSourceFilesMsg rsfpMsg ->
            let
                readSourceFilesReturn =
                    ReadSourceFiles.update
                        rsfpMsg
                        model.readSourceFilesModel

                newModel =
                    { model | readSourceFilesModel = readSourceFilesReturn.model }

                _ =
                    Debug.log "\n\n\nreturn:\n" readSourceFilesReturn.result
            in
                newModel ! [ readSourceFilesReturn.cmd |> Cmd.map ReadSourceFilesMsg ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ externalStop <| always Abort
        , ReadSourceFiles.subscriptions
            |> Sub.map ReadSourceFilesMsg
        ]



-- UTILITIES


delayMsg : Time -> Msg -> Cmd Msg
delayMsg time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)
