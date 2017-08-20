port module ReadSourceFiles exposing (..)

import Dict exposing (Dict)
import Helpers exposing (qualifiedNameToPath)
import Maybe.Extra exposing (isJust)


type alias Model =
    Dict ModuleName ModuleStatus


type alias ModuleStatus =
    { sourceCode : Maybe String
    , dirAttempts : Dict String DirAttempt
    }


type alias DirAttempts =
    Dict String DirAttempt



-- Int
-- { source : Maybe String
-- -- ,
-- )


type alias ModuleName =
    String


type DirAttempt
    = DirNotAttemptedYet
    | InFlight
    | DirSuccess
    | DirFail


type OverallStatus
    = Success { dirName : String }
    | TotalFail
    | HaveNotExhaustedAllOptions { nextDirName : String }


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


init : Model
init =
    Dict.empty


reallyInit : { moduleNames : List String, dirNames : List String } -> Model -> ( Model, Cmd Msg )
reallyInit { moduleNames, dirNames } model =
    let
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
                    { accModuleStatuses = ( moduleName, moduleStatus ) :: accModuleStatuses
                    , accCmds = accCmds
                    }
                else
                    let
                        ( newDirAttempts, cmds ) =
                            getNextCmdsForDirAttempts moduleName dirAttempts
                    in
                        { accModuleStatuses = ( moduleName, { sourceCode = Nothing, dirAttempts = newDirAttempts } ) :: accModuleStatuses
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
                                ( ( moduleName, dirAttempt ), Nothing )
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


getResult : Model -> Maybe (Dict String String)
getResult model =
    if isFinished model then
        model
            |> Dict.map
                (\_ moduleStatus ->
                    moduleStatus.sourceCode |> Maybe.withDefault ""
                )
            |> Just
    else
        Nothing


isFinished : Model -> Bool
isFinished model =
    moduleStatuses model
        |> List.map
            (\( moduleName, status ) ->
                case status of
                    Success _ ->
                        True

                    _ ->
                        False
            )
        |> List.all ((==) True)


subscriptions : Sub Msg
subscriptions =
    readElmModuleResult ReadElmModuleResult


moduleStatus : Model -> ModuleName -> OverallStatus
moduleStatus model moduleName =
    let
        dirAttempts =
            Dict.get moduleName model |> Maybe.map .dirAttempts |> Maybe.withDefault Dict.empty
    in
        if dirAttempts == Dict.empty then
            TotalFail
        else
            case atLeastOneSuccess dirAttempts of
                Just dirName ->
                    Success { dirName = dirName }

                Nothing ->
                    case haveNotExhaustedAllOptions dirAttempts of
                        Just nextDirName ->
                            HaveNotExhaustedAllOptions { nextDirName = nextDirName }

                        Nothing ->
                            TotalFail


moduleStatuses : Model -> List ( String, OverallStatus )
moduleStatuses model =
    model
        |> Dict.toList
        |> List.map
            (\( moduleName, dirAttempts ) -> ( moduleName, moduleStatus model moduleName ))


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        -- _ = Debug.log "\n\nmodel\n" model
        _ =
            Debug.log "ReadSoureFiles.update" True

        _ =
            False
    in
        case msg of
            ReadElmModuleResult { contents, scope } ->
                let
                    _ =
                        Debug.log "scope" scope

                    newModel : Model
                    newModel =
                        model
                            |> (Dict.update
                                    scope.moduleName
                                    (Maybe.map
                                        (\moduleStatus ->
                                            { moduleStatus
                                                | dirAttempts =
                                                    moduleStatus.dirAttempts
                                                        |> Dict.update
                                                            scope.dir
                                                            (updateDirAttempt contents)
                                                , sourceCode = contents
                                            }
                                        )
                                    )
                               )

                    finished =
                        Debug.log "result" (getResult newModel)

                    ( model3, cmds ) =
                        getNextCmds newModel
                in
                    model3 ! cmds


updateDirAttempt : Maybe String -> Maybe DirAttempt -> Maybe DirAttempt
updateDirAttempt maybeSourceCode maybeDirAttempt =
    maybeDirAttempt
        |> Maybe.map
            (\_ ->
                case (Debug.log "maybeSourceCode" maybeSourceCode) of
                    Just sourceCode ->
                        DirSuccess

                    Nothing ->
                        DirFail
            )
