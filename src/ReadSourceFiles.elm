port module ReadSourceFiles exposing (..)

import Dict exposing (Dict)
import Helpers exposing (qualifiedNameToPath)


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
        cmd =
            getNextCmds newModel
    in
        newModel ! (getNextCmds newModel)


getNextCmds : Model -> List (Cmd msg)
getNextCmds model =
    moduleStatuses model
        |> List.map
            (\( moduleName, status ) ->
                case status of
                    HaveNotExhaustedAllOptions { nextDirName } ->
                        let
                            path =
                                nextDirName
                                    ++ "/"
                                    ++ (qualifiedNameToPath moduleName)
                        in
                            Just
                                (readElmModule
                                    { path = path
                                    , scope =
                                        { path = path
                                        , dir = nextDirName
                                        , moduleName = moduleName
                                        }
                                    }
                                )

                    _ ->
                        Nothing
            )
        |> List.filterMap identity


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
                in
                    newModel ! (getNextCmds newModel)


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
