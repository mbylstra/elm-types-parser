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
import Types exposing (DottedModulePath, ModuleInfo, ModuleToModuleInfo, ModuleToSource)


{- REMOVE WHEN COMPILER BUG IS FIXED -}

import Json.Decode


port readElmPackageInfoContents : List String -> Cmd msg


port readElmPackageInfoContentsResult : (List ( String, String ) -> msg) -> Sub msg


port writeOutput : String -> Cmd msg


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
    Dict DottedModulePath
        { relevantNames : List String
        , eitherModuleInfo : EitherModuleInfo
        }


type Msg
    = Stop
    | Abort
    | ReadSourceFilesMsg DottedModulePath ReadSourceFiles.Msg
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


getLoadedModuleInfos : Model -> Dict DottedModulePath ModuleInfo
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


getUnfinishedLoads : Model -> List ReadSourceFiles.Model
getUnfinishedLoads model =
    model.allModulesInfo
        |> Dict.toList
        |> List.map
            (\( moduleName, moduleInfo ) ->
                (case moduleInfo.eitherModuleInfo of
                    Loaded _ ->
                        Nothing

                    NotLoaded rsfModel ->
                        case ReadSourceFiles.hasFailed rsfModel of
                            Just _ ->
                                Nothing

                            Nothing ->
                                Just rsfModel
                )
            )
        |> List.filterMap identity


classifyLoads :
    Model
    -> { loaded : List String, inFlight : List String, failed : List String }
classifyLoads model =
    model.allModulesInfo
        |> Dict.toList
        |> List.foldl
            (\( moduleName, moduleInfo ) ({ inFlight, loaded, failed } as acc) ->
                (case moduleInfo.eitherModuleInfo of
                    Loaded _ ->
                        { inFlight = inFlight, loaded = loaded ++ [ moduleName ], failed = failed }

                    NotLoaded rsfModel ->
                        case ReadSourceFiles.hasFailed rsfModel of
                            Just _ ->
                                { inFlight = inFlight, loaded = loaded, failed = failed ++ [ moduleName ] }

                            Nothing ->
                                { inFlight = inFlight ++ [ moduleName ], loaded = loaded, failed = failed }
                )
            )
            { inFlight = [], loaded = [], failed = [] }


type CurrentProgressState
    = Finished
    | Failed
    | Doing


getCurrentProgressState : Model -> CurrentProgressState
getCurrentProgressState model =
    let
        { inFlight, loaded, failed } =
            classifyLoads model
    in
        if not (List.isEmpty failed) then
            Failed
        else if not (List.isEmpty inFlight) then
            Doing
        else
            Finished


maxCmdsAtOnce : Int
maxCmdsAtOnce =
    3


{-| Lets assume this will only ever be called if the module hasn't been loaded yet
(there's not need to update it once it's loaded)
-}
updateReadSourceFilesModel : Model -> ReadSourceFiles.Model -> Model
updateReadSourceFilesModel model rsfModel =
    { model
        | allModulesInfo =
            model.allModulesInfo
                |> Dict.update
                    rsfModel.moduleName
                    (Maybe.map
                        (\moduleInfo ->
                            case moduleInfo.eitherModuleInfo of
                                Loaded loadedModuleInfo ->
                                    -- We have to "repack" the data. Is there a better way?
                                    { moduleInfo | eitherModuleInfo = Loaded loadedModuleInfo }

                                NotLoaded _ ->
                                    { moduleInfo | eitherModuleInfo = NotLoaded rsfModel }
                        )
                    )
    }



-- |> Helpers.unsafeDictGet rsfModel.moduleName
-- |> .eitherModuleInfo
-- |> (\eitherModule -> case eitherModule of
--     Just loaded ->
--         Loaded _ ->
--
--         NotLoaded _ ->
--             { model
--------------------------------------------------------------------------------
-- UPDATE ----------------------------------------------------------------------
--------------------------------------------------------------------------------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        isTooManyCmdsInFlight =
            getNumCmdsInFlight model >= maxCmdsAtOnce
    in
        -- case (Debug.log "msg" msg) of
        case msg of
            ReadElmPackageInfoContentsResult tupleList ->
                updateWithElmPackageInfoContentsResult tupleList model

            ReadSourceFilesMsg moduleName rsfMsg ->
                handleReadSourceFilesMsg
                    { isTooManyCmdsInFlight = isTooManyCmdsInFlight
                    , model = model
                    , moduleName = moduleName
                    , rsfMsg = rsfMsg
                    }

            Stop ->
                model ! [ exitApp 0 ]

            Abort ->
                model ! [ exitApp -1 ]


handleReadSourceFilesMsg :
    { model : Model
    , moduleName : String
    , rsfMsg : ReadSourceFiles.Msg
    , isTooManyCmdsInFlight : Bool
    }
    -> ( Model, Cmd Msg )
handleReadSourceFilesMsg { model, moduleName, rsfMsg, isTooManyCmdsInFlight } =
    let
        ( newModel, readSourceFilesCmds ) =
            let
                { newAllModulesInfo, newExternalModules, rsfCmds } =
                    updateAllModulesInfoForRsf moduleName rsfMsg isTooManyCmdsInFlight model.allModulesInfo

                ( allModulesInfo2, newExtModulesCmds ) =
                    addNewExternalModules model.sourceDirectories newAllModulesInfo newExternalModules
            in
                ( { model | allModulesInfo = allModulesInfo2 }
                , (rsfCmds |> List.map (Cmd.map (ReadSourceFilesMsg moduleName)))
                    ++ newExtModulesCmds
                )

        writeOutputFileCmd =
            case isFinished newModel of
                True ->
                    let
                        simplifiedAllModuleInfos =
                            simplifyAllModulesInfo newModel.allModulesInfo newModel.subjectModuleInfo

                        output =
                            generateViewFunctions
                                { subjectModuleInfo = newModel.subjectModuleInfo
                                , allModulesInfo = simplifiedAllModuleInfos
                                }
                                |> String.join "\n\n\n\n"

                        _ =
                            Debug.log "Generating Views! Yay!"
                    in
                        writeOutput output

                False ->
                    Cmd.none

        _ =
            case isFinished newModel of
                True ->
                    let
                        simplifiedAllModuleInfos =
                            simplifyAllModulesInfo newModel.allModulesInfo newModel.subjectModuleInfo

                        _ =
                            Debug.log "\n\n\n FINISHED simplifiedAllModuleInfos\n" simplifiedAllModuleInfos

                        _ =
                            Debug.log "\n\n\n FINISHED moduleInfos\n" (getLoadedModuleInfos newModel)

                        _ =
                            Debug.log "\n\n\n FINISHED subjectModuleInfo\n" (newModel.subjectModuleInfo)

                        _ =
                            Debug.log "\n\n\n FINISHED allModulesInfo\n" (newModel.allModulesInfo)
                    in
                        generateViewFunctions
                            { subjectModuleInfo = newModel.subjectModuleInfo
                            , allModulesInfo = simplifiedAllModuleInfos
                            }
                            |> List.map (Debug.log "\n\nviewFunction")

                False ->
                    []

        -- _ =
        --     Debug.log "\n\nfailedLoads" (getFailedLoads newModel)
        _ =
            Debug.log "\n\nmodule load status" (classifyLoads newModel)

        _ =
            Debug.log "\n\nisFinished?" (isFinished newModel)

        ( model3, readSourceFilesCmds2 ) =
            case getCurrentProgressState model of
                Failed ->
                    ( newModel, [] )

                Doing ->
                    if (List.isEmpty readSourceFilesCmds) then
                        let
                            rsfModel =
                                getUnfinishedLoads newModel
                                    |> Helpers.unsafeListHead
                        in
                            ReadSourceFiles.kickBackIntoAction rsfModel
                                |> Tuple.mapFirst (updateReadSourceFilesModel newModel)
                                |> Tuple.mapSecond (List.map <| Cmd.map <| ReadSourceFilesMsg rsfModel.moduleName)
                        -- TODO: generate new comds!
                    else
                        let
                            _ =
                                Debug.log "\n\n\nTHERE ARE STILL CMDS" True
                        in
                            ( newModel, readSourceFilesCmds )

                Finished ->
                    let
                        _ =
                            Debug.log "\n\n\nFINISHED!" True
                    in
                        ( newModel, [] )
    in
        ( model3, Cmd.batch [ readSourceFilesCmds2 |> Cmd.batch, writeOutputFileCmd ] )


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

        -- (_, List (Cmd Msg))
        ( allModulesInfo, readSourceFilesCmds ) =
            model.subjectModuleInfo.externalNamesModuleInfo
                |> groupNamesByModule
                |> List.map
                    (\{ moduleName, relevantNames } ->
                        let
                            -- (_, List (ReadSourceFiles.Cmd Msg))
                            ( rsfModel, rsfCmds ) =
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
                            , rsfCmds |> List.map (Cmd.map (ReadSourceFilesMsg moduleName))
                            )
                    )
                |> List.unzip
                |> Tuple.mapFirst Dict.fromList
                |> Tuple.mapSecond List.concat

        newModel =
            { model
                | packageSourceDirectoriesFound = True
                , sourceDirectories = allSourceDirectories
                , allModulesInfo = allModulesInfo
            }

        -- _ =
        --     Debug.log "allSourceDirectories" allSourceDirectories
        -- _ =
        --     Debug.log "externa;NamesModuleInfo" model.subjectModuleInfo.externalNamesModuleInfo
        _ =
            Debug.log "\n\n\nmodel" model
    in
        ( newModel, readSourceFilesCmds |> Cmd.batch )


updateAllModulesInfoForRsf :
    DottedModulePath
    -> ReadSourceFiles.Msg
    -> Bool
    -> AllModulesInfo
    ->
        { newAllModulesInfo : AllModulesInfo
        , newExternalModules : List { moduleName : DottedModulePath, relevantNames : List String }
        , rsfCmds : List (Cmd ReadSourceFiles.Msg)
        }
updateAllModulesInfoForRsf moduleName rsfMsg isTooManyCmdsInFlight allModulesInfo =
    let
        { relevantNames, eitherModuleInfo } =
            unsafeDictGet "Main.elm line 312" moduleName allModulesInfo
    in
        case eitherModuleInfo of
            NotLoaded oldRsfModel ->
                let
                    { rsfModel, rsfGoal, rsfCmds } =
                        ReadSourceFiles.update
                            rsfMsg
                            isTooManyCmdsInFlight
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

                                newExternalModules : List { moduleName : DottedModulePath, relevantNames : List String }
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
                                , rsfCmds = rsfCmds
                                }

                        Nothing ->
                            { newAllModulesInfo =
                                allModulesInfo
                                    |> Dict.insert moduleName
                                        { relevantNames = relevantNames
                                        , eitherModuleInfo = NotLoaded rsfModel
                                        }
                            , newExternalModules = []
                            , rsfCmds = rsfCmds
                            }

            Loaded _ ->
                { newAllModulesInfo = allModulesInfo
                , newExternalModules = []
                , rsfCmds = []
                }


addNewExternalModules :
    List String
    -> AllModulesInfo
    -> List { moduleName : DottedModulePath, relevantNames : List String }
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
                            ( rsfModel, rsfCmds ) =
                                ReadSourceFiles.init
                                    { sourceDirectories = sourceDirectories
                                    , moduleName = moduleName
                                    }

                            mappedCmds =
                                rsfCmds |> List.map (Cmd.map (ReadSourceFilesMsg moduleName))
                        in
                            { accAllModulesInfo =
                                accAllModulesInfo
                                    |> Dict.insert moduleName
                                        { relevantNames = relevantNames
                                        , eitherModuleInfo = NotLoaded rsfModel
                                        }
                            , accCmds = accCmds ++ mappedCmds
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
    -> ModuleInfo
    -> Dict DottedModulePath ModuleInfo
simplifyAllModulesInfo allModulesInfo subjectModuleInfo =
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
        |> Dict.insert subjectModuleInfo.dottedModulePath subjectModuleInfo


getNumCmdsInFlight : Model -> Int
getNumCmdsInFlight model =
    model.allModulesInfo
        |> Dict.values
        |> List.map .eitherModuleInfo
        |> List.map
            (\eitherModuleInfo ->
                case eitherModuleInfo of
                    NotLoaded readSourceFilesModel ->
                        ReadSourceFiles.numCmdsInFlight readSourceFilesModel

                    _ ->
                        0
            )
        |> List.sum
