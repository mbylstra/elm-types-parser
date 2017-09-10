port module Main exposing (..)

-- import Dict exposing (Dict)

import Json.Decode
import PackageInfo exposing (PackageInfo)
import ReadSourceFiles
import DeterminePackageLocations
import Types exposing (ModuleInfo, ModuleToSource, ModuleToModuleInfo, DottedModuleName)
import SubjectModuleInfo
import ModuleInfo exposing (groupNamesByModule)
import DependentModules
import Dict exposing (Dict)
import Helpers exposing (unsafeDictGet)


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
    , subjectSourceCode : String
    , subjectModuleInfo : ModuleInfo
    , allModulesInfo : AllModulesInfo
    }


type EitherModuleInfo
    = Loaded ModuleInfo
    | NotLoaded ReadSourceFiles.Model


type alias AllModulesInfo =
    Dict DottedModuleName
        { relevantNames : List String
        , eitherModuleInfo : EitherModuleInfo
        }


type Msg
    = Stop
    | Abort
    | ReadSourceFilesMsg DottedModuleName ReadSourceFiles.Msg


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

                    subjectModuleInfo : ModuleInfo
                    subjectModuleInfo =
                        subjectSourceCode
                            |> SubjectModuleInfo.getModuleInfo

                    ( allModulesInfo, readSourceFilesCmds ) =
                        subjectModuleInfo.externalNamesModuleInfo
                            |> groupNamesByModule
                            |> List.map
                                (\{ moduleName, relevantNames } ->
                                    let
                                        ( rsfModel, rsfCmd ) =
                                            ReadSourceFiles.init
                                                { sourceDirectories = sourceDirectories
                                                , moduleName = moduleName
                                                }
                                    in
                                        ( ( moduleName
                                          , { relevantNames = relevantNames
                                            , eitherModuleInfo = NotLoaded rsfModel
                                            }
                                          )
                                        , rsfCmd |> Cmd.map (ReadSourceFilesMsg moduleName)
                                        )
                                )
                            |> List.unzip
                            |> Tuple.mapFirst Dict.fromList

                    -- we need to store this in the model, so that once we've read the source files,
                    -- we can match it
                    modulesToLoad =
                        subjectModuleInfo
                            |> ModuleInfo.getModulesToLoad

                    -- ( readSourceFilesModel, readSourceFilesCmd ) =
                    --     ReadSourceFiles.init
                    --         { sourceDirectories = sourceDirectories
                    --         , moduleNames = modulesToLoad
                    --         }
                    -- ( readSourceFilesModels, readSourceFilesCmds ) =
                    --     modulesToLoad
                    --         |> List.map
                    --             (\moduleName ->
                    --             )
                    -- allModulesInfo : AllModulesInfo
                    -- allModulesInfo =
                    --     readSourceFilesModels
                    --         |> List.map
                    --             (\({ moduleName } as readSourceFileModel) ->
                    --                 ( moduleName, NotLoaded readSourceFileModel )
                    --             )
                    --         |> Dict.fromList
                in
                    { subjectSourceCode = subjectSourceCode
                    , sourceDirectories = sourceDirectories
                    , subjectModuleInfo = subjectModuleInfo
                    , allModulesInfo = allModulesInfo
                    }
                        ! readSourceFilesCmds

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

        ReadSourceFilesMsg moduleName rsfMsg ->
            let
                { newAllModulesInfo, newExternalModules, rsfCmd } =
                    updateAllModulesInfoForRsf moduleName rsfMsg model.allModulesInfo

                ( allModulesInfo2, newExtModulesCmds ) =
                    addNewExternalModules model.sourceDirectories newAllModulesInfo newExternalModules
            in
                { model | allModulesInfo = allModulesInfo2 }
                    ! ([ rsfCmd |> Cmd.map (ReadSourceFilesMsg moduleName)
                       ]
                        ++ newExtModulesCmds
                      )


updateAllModulesInfoForRsf :
    DottedModuleName
    -> ReadSourceFiles.Msg
    -> AllModulesInfo
    ->
        { newAllModulesInfo : AllModulesInfo
        , newExternalModules : List { moduleName : DottedModuleName, relevantNames : List String }
        , rsfCmd : Cmd ReadSourceFiles.Msg
        }
updateAllModulesInfoForRsf moduleName rsfMsg allModulesInfo =
    let
        { relevantNames, eitherModuleInfo } =
            unsafeDictGet moduleName allModulesInfo
    in
        case eitherModuleInfo of
            NotLoaded oldRsfModel ->
                let
                    { rsfModel, rsfGoal, rsfCmd } =
                        ReadSourceFiles.update
                            rsfMsg
                            oldRsfModel
                in
                    case rsfGoal of
                        Just sourceCode ->
                            let
                                moduleInfo : ModuleInfo
                                moduleInfo =
                                    DependentModules.getModuleInfo
                                        { sourceCode = sourceCode
                                        , relevantNames = relevantNames
                                        }

                                newExternalModules :
                                    List
                                        { moduleName : DottedModuleName
                                        , relevantNames : List String
                                        }
                                newExternalModules =
                                    groupNamesByModule moduleInfo.externalNamesModuleInfo
                            in
                                { newAllModulesInfo =
                                    allModulesInfo
                                        |> Dict.insert moduleName
                                            { relevantNames = relevantNames
                                            , eitherModuleInfo = Loaded moduleInfo
                                            }
                                , newExternalModules = newExternalModules
                                , rsfCmd = rsfCmd
                                }

                        Nothing ->
                            { newAllModulesInfo =
                                allModulesInfo
                                    |> Dict.insert moduleName
                                        { relevantNames = relevantNames
                                        , eitherModuleInfo = NotLoaded rsfModel
                                        }
                            , newExternalModules = []
                            , rsfCmd = rsfCmd
                            }

            Loaded _ ->
                { newAllModulesInfo = allModulesInfo
                , newExternalModules = []
                , rsfCmd = Cmd.none
                }


addNewExternalModules :
    List String
    -> AllModulesInfo
    -> List { moduleName : DottedModuleName, relevantNames : List String }
    -> ( AllModulesInfo, List (Cmd Msg) )
addNewExternalModules sourceDirectories allModulesInfo newExternalModules =
    -- this is really naive as it assumes we only pass over a module once.
    -- it will need to merge "relevantNames" and potentially generate new msgs
    -- if there are new "relevantNames"
    newExternalModules
        |> List.foldl
            (\{ moduleName, relevantNames } ({ accAllModulesInfo, accCmds } as acc) ->
                case Dict.get moduleName allModulesInfo of
                    Just moduleInfo ->
                        -- this is where we need to do tricky stuff, but for
                        -- now we do nothing.
                        acc

                    Nothing ->
                        let
                            ( rsfModel, rsfCmd ) =
                                ReadSourceFiles.init
                                    { sourceDirectories = sourceDirectories
                                    , moduleName = moduleName
                                    }

                            mappedCmd =
                                rsfCmd |> Cmd.map (ReadSourceFilesMsg moduleName)
                        in
                            { accAllModulesInfo =
                                accAllModulesInfo
                                    |> Dict.insert moduleName
                                        { relevantNames = relevantNames
                                        , eitherModuleInfo = NotLoaded rsfModel
                                        }
                            , accCmds = mappedCmd :: accCmds
                            }
            )
            { accAllModulesInfo = allModulesInfo
            , accCmds = []
            }
        |> (\{ accAllModulesInfo, accCmds } ->
                ( accAllModulesInfo, accCmds )
           )


subscriptions : Model -> Sub Msg
subscriptions model =
    ((externalStop <| always Abort)
        -- :: [ ReadSourceFiles.subscriptions |> Sub.map ReadSourceFilesMsg ]
        :: []
    )
        |> Sub.batch
