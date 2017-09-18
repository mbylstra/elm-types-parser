port module Main exposing (..)

-- import Dict exposing (Dict)

import DataGeneration exposing (generateViewFunctions)
import DependentModules
import DeterminePackageLocations
import Dict exposing (Dict)
import Helpers exposing (allTrue, unsafeAssumeSuccess, unsafeDictGet)
import Json.Decode
import ModuleInfo exposing (groupNamesByModule)
import PackageInfo exposing (PackageInfo)
import Path.Posix as Path exposing (dropFileName, joinPath, takeDirectory)
import ReadSourceFiles
import SubjectModuleInfo
import Types exposing (DottedModuleName, ModuleInfo, ModuleToModuleInfo, ModuleToSource)


{- REMOVE WHEN COMPILER BUG IS FIXED -}

import Json.Decode


port readElmPackageInfoContents : List String -> Cmd msg


port readElmPackageInfoContentsResult : (List ( String, String ) -> msg) -> Sub msg


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
    { packageSourceDirectoriesFound : Bool
    , sourceDirectories : List String
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
    | ReadElmPackageInfoContentsResult (List ( String, String ))


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
                        DeterminePackageLocations.getPackagePaths exactDependenciesContents

                    sourceDirectories =
                        packageInfo.sourceDirectories ++ packageDirs

                    subjectModuleInfo : ModuleInfo
                    subjectModuleInfo =
                        subjectSourceCode
                            |> SubjectModuleInfo.getModuleInfo

                    elmPackageJsonPaths =
                        packageDirs
                            |> List.map
                                (\packageDir ->
                                    joinPath [ packageDir, "elm-package.json" ]
                                )
                in
                    { packageSourceDirectoriesFound = False
                    , subjectSourceCode = subjectSourceCode
                    , sourceDirectories = sourceDirectories
                    , subjectModuleInfo = subjectModuleInfo
                    , allModulesInfo = Dict.empty
                    }
                        ! [ readElmPackageInfoContents elmPackageJsonPaths ]

            Err err ->
                let
                    err2 =
                        "Invalid elm-package.json.\n\n " ++ err
                in
                    Debug.crash err2


isFinished : Model -> Bool
isFinished model =
    model.allModulesInfo
        |> Dict.values
        |> List.map
            (.eitherModuleInfo
                >> (\eitherModuleInfo ->
                        case eitherModuleInfo of
                            Loaded _ ->
                                True

                            NotLoaded _ ->
                                False
                   )
            )
        |> allTrue


getLoadedModuleInfos : Model -> Dict DottedModuleName ModuleInfo
getLoadedModuleInfos model =
    model.allModulesInfo
        |> Dict.toList
        |> List.map
            (\( moduleName, moduleInfo ) ->
                (case moduleInfo.eitherModuleInfo of
                    Loaded moduleInfo ->
                        Just ( moduleName, moduleInfo )

                    NotLoaded _ ->
                        Nothing
                )
            )
        |> List.filterMap identity
        |> Dict.fromList


getFailedLoads : Model -> List ReadSourceFiles.Model
getFailedLoads model =
    model.allModulesInfo
        |> Dict.toList
        |> List.map
            (\( moduleName, moduleInfo ) ->
                (case moduleInfo.eitherModuleInfo of
                    Loaded _ ->
                        Nothing

                    NotLoaded rsfModel ->
                        ReadSourceFiles.hasFailed rsfModel
                )
            )
        |> List.filterMap identity


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case (Debug.log "msg" msg) of
        -- case msg of
        ReadElmPackageInfoContentsResult tupleList ->
            updateWithElmPackageInfoContentsResult tupleList model

        ReadSourceFilesMsg moduleName rsfMsg ->
            let
                ( newModel, cmd ) =
                    let
                        { newAllModulesInfo, newExternalModules, rsfCmd } =
                            updateAllModulesInfoForRsf moduleName rsfMsg model.allModulesInfo

                        ( allModulesInfo2, newExtModulesCmds ) =
                            addNewExternalModules model.sourceDirectories newAllModulesInfo newExternalModules

                        _ =
                            Debug.log ("ReadSourceFilesMsg for " ++ moduleName) True
                    in
                        { model | allModulesInfo = allModulesInfo2 }
                            ! ([ rsfCmd |> Cmd.map (ReadSourceFilesMsg moduleName)
                               ]
                                ++ newExtModulesCmds
                              )

                -- _ =
                --     case isFinished newModel of
                --         True ->
                --             let
                --                 _ =
                --                     Debug.log "\n\n\n FINISHED moduleInfos\n" (getLoadedModuleInfos newModel)
                --
                --                 _ =
                --                     Debug.log "\n\n\n FINISHED subjectModuleInfo\n" (newModel.subjectModuleInfo)
                --             in
                --                 generateViewFunctions
                --                     { subjectModuleInfo = newModel.subjectModuleInfo
                --                     , allModulesInfo = simplifyAllModulesInfo newModel.allModulesInfo
                --                     }
                --                     |> List.map (Debug.log "viewFunction")
                --
                --         False ->
                --             []
                _ =
                    Debug.log "failedLoads" (getFailedLoads newModel)

                _ =
                    Debug.log "isFinished" (isFinished newModel)
            in
                ( newModel, cmd )

        Stop ->
            model ! [ exitApp 0 ]

        Abort ->
            model ! [ exitApp -1 ]


updateWithElmPackageInfoContentsResult : List ( String, String ) -> Model -> ( Model, Cmd Msg )
updateWithElmPackageInfoContentsResult tupleList model =
    let
        packageSourceDirectories =
            tupleList
                |> List.concatMap
                    (\( elmPackagePath, contents ) ->
                        Json.Decode.decodeString PackageInfo.decoder contents
                            -- for the decoder to fail there'd have to be a completely
                            -- broken package installed. We wouldn't even get this
                            -- far if that were the case.
                            |> unsafeAssumeSuccess
                            |> .sourceDirectories
                            |> List.map
                                (\relativeSourceDirectory ->
                                    joinPath
                                        [ dropFileName elmPackagePath
                                        , relativeSourceDirectory
                                        ]
                                )
                    )

        allSourceDirectories =
            model.sourceDirectories ++ packageSourceDirectories

        ( allModulesInfo, readSourceFilesCmds ) =
            model.subjectModuleInfo.externalNamesModuleInfo
                |> groupNamesByModule
                |> List.map
                    (\{ moduleName, relevantNames } ->
                        let
                            ( rsfModel, rsfCmd ) =
                                ReadSourceFiles.init
                                    { sourceDirectories = allSourceDirectories
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

        newModel =
            { model
                | packageSourceDirectoriesFound = True
                , sourceDirectories = allSourceDirectories
                , allModulesInfo = allModulesInfo
            }

        _ =
            Debug.log "\n\nnewModel after updateWithElmPackageInfoContentsResult" newModel
    in
        newModel ! readSourceFilesCmds


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
            unsafeDictGet "Main.elm line 312" moduleName allModulesInfo
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
    ([ externalStop <| always Abort ]
        ++ [ readElmPackageInfoContentsResult ReadElmPackageInfoContentsResult ]
        ++ [ readSourceFilesSubscription ]
    )
        |> Sub.batch


readSourceFilesSubscription : Sub Msg
readSourceFilesSubscription =
    ReadSourceFiles.subscription
        |> Sub.map (\( moduleName, rsfMsg ) -> (ReadSourceFilesMsg moduleName rsfMsg))


simplifyAllModulesInfo :
    AllModulesInfo
    -> Dict DottedModuleName ModuleInfo
simplifyAllModulesInfo allModulesInfo =
    allModulesInfo
        |> Dict.toList
        |> List.filterMap
            (\( dottedModuleName, { relevantNames, eitherModuleInfo } ) ->
                case eitherModuleInfo of
                    Loaded moduleInfo ->
                        Just ( dottedModuleName, moduleInfo )

                    NotLoaded _ ->
                        Nothing
            )
        |> Dict.fromList



-- |> Dict.fromList
