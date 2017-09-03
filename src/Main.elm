port module Main exposing (..)

-- import Dict exposing (Dict)

import Json.Decode
import PackageInfo exposing (PackageInfo)
import Process
import ReadSourceFiles
import Task
import Time exposing (Time)
import DeterminePackageLocations
import Types exposing (ModuleInfo, ModuleToSource)
import SubjectModuleInfo


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


type ProgramStage
    = LoadingTheSubjectsDependentModules
    | LoadingAllDependentModules { subjectDependentModuleSource : ModuleToSource }
    | FinishedLoadingModules


type alias Model =
    { programStage : ProgramStage
    , sourceDirectories : List String
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
                            |> SubjectModuleInfo.getModuleInfo

                    -- we need to store this in the model, so that once we've read the source files,
                    -- we can match it
                    modulesToLoad =
                        subjectModuleInfo
                            |> SubjectModuleInfo.getModulesToLoad

                    ( readSourceFilesModel, readSourceFilesCmd ) =
                        ReadSourceFiles.init
                            { dirNames = packageDirs ++ sourceDirectories
                            , moduleNames = modulesToLoad
                            }
                in
                    { programStage = LoadingTheSubjectsDependentModules
                    , subjectSourceCode = subjectSourceCode
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
update msg model1 =
    case msg of
        Stop ->
            model1 ! [ exitApp 0 ]

        Abort ->
            model1 ! [ exitApp -1 ]

        ReadSourceFilesMsg rsfMsg ->
            let
                { rsfModel, rsfGoal, rsfCmd } =
                    ReadSourceFiles.update
                        rsfMsg
                        model1.readSourceFilesModel

                model2 =
                    { model1 | readSourceFilesModel = rsfModel }

                model3 =
                    case rsfGoal of
                        Just moduleToSource ->
                            { model2
                                | programStage =
                                    LoadingAllDependentModules
                                        { subjectDependentModuleSource = moduleToSource }
                            }

                        Nothing ->
                            model2

                _ =
                    Debug.log "\n\ngoal:\n" rsfGoal
            in
                model3 ! [ rsfCmd |> Cmd.map ReadSourceFilesMsg ]


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
