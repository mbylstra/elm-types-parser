port module Main exposing (..)

-- import Dict exposing (Dict)

import Json.Decode
import PackageInfo exposing (PackageInfo)
import Process
import ReadSourceFiles
import Task
import Time exposing (Time)
import DeterminePackageLocations
import Types exposing (ModuleInfo, ModuleToSource, ModuleToModuleInfo)
import SubjectModuleInfo
import ModuleInfo
import DependentModules
import Dict


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
    { programStage : ProgramStage
    , sourceDirectories : List String
    , readSourceFilesModel : ReadSourceFiles.Model
    , subjectSourceCode : String
    , subjectModuleInfo : ModuleInfo
    }


type ProgramStage
    = LoadingTheSubjectsDependentModules
    | LoadingAllDependentModules
        { moduleInfos : ModuleToModuleInfo
        , readSourceFilesModel : ReadSourceFiles.Model

        -- when we get a ReadSourceFilesMsg, we need to make sure it gets routed to this somehow :/
        }
    | FinishedLoadingModules


type Msg
    = Stop
    | Abort
    | ReadSourceFilesMsg ReadSourceFiles.Msg
    | LoadingAllDependentModulesMsg LoadingAllDependentModulesMsg


type LoadingAllDependentModulesMsg
    = LADMReadSourceFilesMsg ReadSourceFiles.Msg


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
                        packageInfo.sourceDirectories ++ packageDirs

                    subjectModuleInfo =
                        subjectSourceCode
                            |> SubjectModuleInfo.getModuleInfo

                    -- we need to store this in the model, so that once we've read the source files,
                    -- we can match it
                    modulesToLoad =
                        subjectModuleInfo
                            |> ModuleInfo.getModulesToLoad

                    ( readSourceFilesModel, readSourceFilesCmd ) =
                        ReadSourceFiles.init
                            { sourceDirectories = sourceDirectories
                            , moduleNames = modulesToLoad
                            }
                in
                    { programStage = LoadingTheSubjectsDependentModules
                    , subjectSourceCode = subjectSourceCode
                    , sourceDirectories = sourceDirectories
                    , readSourceFilesModel = readSourceFilesModel
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
    -- case (Debug.log "msg" msg) of
    case msg of
        Stop ->
            model ! [ exitApp 0 ]

        Abort ->
            model ! [ exitApp -1 ]

        ReadSourceFilesMsg rsfMsg ->
            let
                _ =
                    Debug.log "initial ReadSourceFilesMsg" True

                { rsfModel, rsfGoal, rsfCmd } =
                    ReadSourceFiles.update
                        rsfMsg
                        model.readSourceFilesModel

                model2 =
                    { model | readSourceFilesModel = rsfModel }

                ( model3, dependentRsfCmd ) =
                    case rsfGoal of
                        Just moduleToSource ->
                            let
                                _ =
                                    Debug.log "subject ReadSourceFiles goal keys" (Dict.keys moduleToSource)

                                usedSymbols =
                                    ModuleInfo.getExternalSymbols model2.subjectModuleInfo

                                moduleInfos =
                                    DependentModules.getModuleInfos
                                        { moduleToSource = moduleToSource, usedSymbols = usedSymbols }

                                -- now that we have the dependent module infos,
                                -- we need to go through each of them and download their dependent modules
                                -- (we should take care to not do the same thing more than once, but as MVP
                                -- its ok)
                                modulesToLoad : List String
                                modulesToLoad =
                                    moduleInfos
                                        |> Dict.values
                                        |> List.concatMap ModuleInfo.getModulesToLoad

                                ( readSourceFilesModel, dependentRsfCmd ) =
                                    ReadSourceFiles.init
                                        { sourceDirectories = model2.sourceDirectories
                                        , moduleNames = (Debug.log "modulesToLoad" modulesToLoad)
                                        }
                            in
                                ( { model2
                                    | programStage =
                                        LoadingAllDependentModules
                                            { moduleInfos = moduleInfos
                                            , readSourceFilesModel = readSourceFilesModel
                                            }
                                  }
                                , dependentRsfCmd
                                )

                        Nothing ->
                            ( model2, Cmd.none )

                -- _ =
                --     Debug.log "\n\ngoal:\n" rsfGoal
                _ =
                    Debug.log "rsfCmd" rsfCmd

                _ =
                    Debug.log "dependentRsfCmd" dependentRsfCmd
            in
                model3
                    -- ! [ rsfCmd |> Cmd.map ReadSourceFilesMsg
                    --   , dependentRsfCmd |> Cmd.map (LoadingAllDependentModulesMsg << LADMReadSourceFilesMsg)
                    --   ]
                    ! [ dependentRsfCmd |> Cmd.map (LoadingAllDependentModulesMsg << LADMReadSourceFilesMsg)
                      ]

        -- | LoadingAllDependentModulesMsg LoadingAllDependentModulesMsg
        -- = LADMReadSourceFilesMsg ReadSourceFiles.Msg
        LoadingAllDependentModulesMsg ladmMsg ->
            case model.programStage of
                LoadingAllDependentModules ladmModel ->
                    case ladmMsg of
                        LADMReadSourceFilesMsg rsfMsg ->
                            let
                                { rsfModel, rsfGoal, rsfCmd } =
                                    ReadSourceFiles.update rsfMsg ladmModel.readSourceFilesModel

                                _ =
                                    Debug.log "rsfGoal" rsfGoal

                                _ =
                                    case rsfGoal of
                                        Just goal ->
                                            Debug.log "goal keys" (Dict.keys goal)

                                        Nothing ->
                                            []
                            in
                                { model
                                    | programStage =
                                        LoadingAllDependentModules
                                            { ladmModel | readSourceFilesModel = rsfModel }
                                }
                                    ! []

                _ ->
                    model ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    (externalStop <| always Abort)
        :: (case model.programStage of
                LoadingTheSubjectsDependentModules ->
                    [ ReadSourceFiles.subscriptions |> Sub.map ReadSourceFilesMsg ]

                LoadingAllDependentModules _ ->
                    [ ReadSourceFiles.subscriptions |> Sub.map (LoadingAllDependentModulesMsg << LADMReadSourceFilesMsg) ]

                FinishedLoadingModules ->
                    []
           )
        |> Sub.batch



-- UTILITIES


delayMsg : Time -> Msg -> Cmd Msg
delayMsg time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)
