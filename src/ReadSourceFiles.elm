port module ReadSourceFiles exposing (..)

import Dict exposing (Dict)
import Helpers exposing (qualifiedNameToPath)
import Maybe.Extra exposing (isJust)
import Types exposing (DottedModuleName, ModuleToSource, SourceCode)


type alias Model =
    Dict DottedModuleName ModuleStatus



-- what is modulename exactly?


type alias ModuleStatus =
    { sourceCode : Maybe String
    , dirAttempts : Dict String DirAttempt
    }


type alias DirAttempts =
    Dict DirPath DirAttempt


type alias DirPath =
    String



-- Int
-- { source : Maybe String
-- -- ,
-- )


type DirAttempt
    = DirNotAttemptedYet
    | InFlight
    | DirSuccess
    | DirFail


type alias ReadElmModuleScope =
    { path : String, dir : String, moduleName : String }


type Msg
    = ReadElmModuleResult ReadElmModuleResultR


type alias ReadElmModuleResultR =
    { contents : Maybe String
    , scope : ReadElmModuleScope
    }


type alias GetFilenamesInDirResultR =
    { filenames : Maybe (List String)
    , scope : ReadElmModuleScope
    }


type alias GetFilenamesInDirResultScope =
    ()


port readElmModule :
    { path : String
    , scope : ReadElmModuleScope
    }
    -> Cmd msg


port readElmModuleResult : (ReadElmModuleResultR -> msg) -> Sub msg



-- port getFilenamesInDirReturned : (? -> msg) -> Sub msg


init : { moduleNames : List String, dirNames : List String } -> ( Model, Cmd Msg )
init { moduleNames, dirNames } =
    let
        _ =
            Debug.log "dirNames" dirNames

        newModel =
            (moduleNames
                |> List.map
                    (\moduleName ->
                        ( moduleName
                        , { dirAttempts =
                                dirNames
                                    |> List.map
                                        (((,) |> flip) DirNotAttemptedYet)
                                    |> Dict.fromList

                          -- (\dirName ->
                          --     { dirName = dirName, status = DirNotAttemptedYet }
                          -- )
                          , sourceCode = Nothing
                          }
                        )
                    )
                |> Dict.fromList
            )

        -- _ =
        --     Debug.log "\n\nreadSourceFiles newModel\n\n" newModel
        ( model3, cmds ) =
            getNextCmds newModel
    in
        model3 ! cmds


getNextCmds : Model -> ( Model, List (Cmd Msg) )
getNextCmds model =
    model
        |> Dict.toList
        |> List.foldl
            (\( moduleName, { sourceCode, dirAttempts } as moduleStatus ) { accModuleStatuses, accCmds } ->
                if isJust sourceCode then
                    { accModuleStatuses =
                        ( moduleName, moduleStatus ) :: accModuleStatuses
                    , accCmds = accCmds
                    }
                else
                    let
                        ( newDirAttempts, cmds ) =
                            getNextCmdsForDirAttempts moduleName dirAttempts
                    in
                        { accModuleStatuses =
                            ( moduleName
                            , { sourceCode = Nothing, dirAttempts = newDirAttempts }
                            )
                                :: accModuleStatuses
                        , accCmds = cmds ++ accCmds
                        }
            )
            { accModuleStatuses = [], accCmds = [] }
        |> (\{ accModuleStatuses, accCmds } ->
                ( accModuleStatuses |> Dict.fromList
                , accCmds
                )
           )


getNextCmdsForDirAttempts : String -> DirAttempts -> ( DirAttempts, List (Cmd Msg) )
getNextCmdsForDirAttempts moduleName originalDirAttempts =
    originalDirAttempts
        |> Dict.toList
        |> List.foldl
            (\( dirName, dirAttempt ) { dirAttempts, maybeCmds } ->
                let
                    _ =
                        Debug.log "dirName" dirName

                    path : String
                    path =
                        dirName
                            ++ "/"
                            ++ (qualifiedNameToPath moduleName)

                    ( newDirAttempt, maybeCmd ) =
                        case dirAttempt of
                            DirNotAttemptedYet ->
                                ( ( dirName, InFlight )
                                , Just
                                    (readElmModule
                                        { path = path
                                        , scope =
                                            { path = path
                                            , dir = dirName
                                            , moduleName = moduleName
                                            }
                                        }
                                    )
                                )

                            _ ->
                                ( ( dirName, dirAttempt ), Nothing )

                    _ =
                        Debug.log "newDirAttempt" newDirAttempt
                in
                    { dirAttempts = newDirAttempt :: dirAttempts
                    , maybeCmds = maybeCmd :: maybeCmds
                    }
            )
            { dirAttempts = [], maybeCmds = [] }
        |> (\{ dirAttempts, maybeCmds } ->
                ( dirAttempts |> Dict.fromList
                , maybeCmds |> List.filterMap identity
                )
           )



-- what is the first string in the dict exactly?
-- its moduleName, modul


getResult : Model -> Result Model (Dict String String)
getResult model =
    if isFinished model then
        model
            |> Dict.map
                (\_ moduleStatus ->
                    moduleStatus.sourceCode |> Maybe.withDefault ""
                )
            |> Ok
    else
        model
            |> Dict.toList
            |> List.filterMap
                (\( moduleName, moduleStatus ) ->
                    if isJust moduleStatus.sourceCode then
                        Nothing
                    else
                        Just ( moduleName, moduleStatus )
                )
            |> Dict.fromList
            |> Err


isFinished : Model -> Bool
isFinished model =
    moduleStatuses model
        |> List.map Tuple.second
        |> List.all ((==) True)


subscriptions : Sub Msg
subscriptions =
    readElmModuleResult ReadElmModuleResult


isModuleFinished : Model -> DottedModuleName -> Bool
isModuleFinished model moduleName =
    Dict.get moduleName model
        |> Maybe.map .sourceCode
        |> Maybe.withDefault Nothing
        |> isJust


moduleStatuses : Model -> List ( String, Bool )
moduleStatuses model =
    model
        |> Dict.toList
        |> List.map
            (\( moduleName, dirAttempts ) -> ( moduleName, isModuleFinished model moduleName ))


atLeastOneSuccess : DirAttempts -> Maybe String
atLeastOneSuccess dirAttempts =
    dirAttempts
        |> Dict.toList
        |> List.map
            (\( dir, attempt ) ->
                case attempt of
                    DirSuccess ->
                        Just dir

                    _ ->
                        Nothing
            )
        |> List.filterMap identity
        |> List.head


haveNotExhaustedAllOptions : DirAttempts -> Maybe String
haveNotExhaustedAllOptions dirAttempts =
    dirAttempts
        |> Dict.toList
        |> List.map
            (\( dir, attempt ) ->
                case attempt of
                    DirNotAttemptedYet ->
                        Just dir

                    _ ->
                        Nothing
            )
        |> List.filterMap identity
        |> List.head



-- what is Dict String String?


update : Msg -> Model -> { model : Model, result : Maybe ModuleToSource, cmd : Cmd Msg }
update msg model =
    let
        _ =
            Debug.log "\n\nmodel\n" model

        -- _ =
        --     Debug.log "ReadSoureFiles.update" True
        _ =
            False
    in
        case msg of
            ReadElmModuleResult { contents, scope } ->
                let
                    -- _ =
                    --     Debug.log "scope" scope
                    newModel : Model
                    newModel =
                        model
                            |> (Dict.update
                                    scope.moduleName
                                    (\maybeExistingValue ->
                                        case maybeExistingValue of
                                            Just moduleStatus ->
                                                Just <|
                                                    let
                                                        _ =
                                                            if not <| isJust <| Dict.get scope.dir moduleStatus.dirAttempts then
                                                                Debug.crash ("could not find " ++ scope.dir ++ " for " ++ scope.moduleName)
                                                            else
                                                                ()
                                                    in
                                                        { moduleStatus
                                                            | dirAttempts =
                                                                moduleStatus.dirAttempts
                                                                    |> Dict.update
                                                                        scope.dir
                                                                        (updateDirAttempt contents)
                                                            , sourceCode = contents
                                                        }

                                            Nothing ->
                                                Debug.crash ("could not update module " ++ scope.moduleName)
                                    )
                               )

                    ( model3, cmds ) =
                        getNextCmds newModel

                    result =
                        getResult model3

                    -- _ =
                    --     Debug.log "RESULT ****************\n\n" (result |> Result.map Dict.keys)
                in
                    { model = model3, cmd = Cmd.batch cmds, result = result |> Result.toMaybe }


updateDirAttempt : Maybe String -> Maybe DirAttempt -> Maybe DirAttempt
updateDirAttempt maybeSourceCode maybeDirAttempt =
    (Debug.log "maybeDirAttempt" maybeDirAttempt)
        |> Maybe.map
            (\_ ->
                case maybeSourceCode of
                    Just sourceCode ->
                        DirSuccess

                    Nothing ->
                        DirFail
            )
