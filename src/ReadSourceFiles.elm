port module ReadSourceFiles exposing (..)

import Dict exposing (Dict)
import Helpers exposing (qualifiedNameToPath)
import Maybe.Extra exposing (isJust, isNothing)
import Types exposing (DottedModuleName, ModuleToSource, SourceCode)


type alias Model =
    { moduleName : String
    , sourceCode : Maybe String
    , dirAttempts : Dict String DirAttempt
    }



-- type alias ModuleStatus =


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


type alias ReadElmModulePortScope =
    { path : String, dir : String, moduleName : String }


type Msg
    = ReadElmModuleResult ReadElmModuleResultR


type alias ReadElmModuleResultR =
    { contents : Maybe String
    , portScope : ReadElmModulePortScope
    }


type alias GetFilenamesInDirResultR =
    { filenames : Maybe (List String)
    , portScope : ReadElmModulePortScope
    }


type alias GetFilenamesInDirResultScope =
    ()


port readElmModule :
    { path : String
    , portScope : ReadElmModulePortScope
    }
    -> Cmd msg


port readElmModuleResult : (ReadElmModuleResultR -> msg) -> Sub msg



-- port getFilenamesInDirReturned : (? -> msg) -> Sub msg


init : { moduleName : String, sourceDirectories : List String } -> ( Model, Cmd Msg )
init { moduleName, sourceDirectories } =
    let
        model =
            { moduleName = moduleName
            , dirAttempts =
                sourceDirectories
                    |> List.map
                        (((,) |> flip) DirNotAttemptedYet)
                    |> Dict.fromList
            , sourceCode = Nothing
            }

        ( model2, cmds ) =
            getNextCmds model
    in
        model2 ! cmds


getNextCmds : Model -> ( Model, List (Cmd Msg) )
getNextCmds ({ moduleName, sourceCode, dirAttempts } as model) =
    case sourceCode of
        Just _ ->
            ( model, [] )

        Nothing ->
            let
                ( newDirAttempts, cmds ) =
                    getNextCmdsForDirAttempts moduleName dirAttempts
            in
                ( { model | dirAttempts = newDirAttempts }, cmds )


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
                                        , portScope =
                                            { path = path
                                            , dir = dirName
                                            , moduleName = moduleName
                                            }
                                        }
                                    )
                                )

                            _ ->
                                ( ( dirName, dirAttempt ), Nothing )
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


getGoal : Model -> Result Model SourceCode
getGoal model =
    if isFinished model then
        Ok (model.sourceCode |> Maybe.withDefault "")
    else
        Err model


subscription : Sub ( DottedModuleName, Msg )
subscription =
    readElmModuleResult ReadElmModuleResult
        |> Sub.map
            (\result ->
                case result of
                    ReadElmModuleResult resultR ->
                        ( resultR.portScope.moduleName, result )
            )


isFinished : Model -> Bool
isFinished model =
    isJust model.sourceCode



-- moduleStatuses : Model -> List ( String, Bool )
-- moduleStatuses model =
--     model
--         |> Dict.toList
--         |> List.map
--             (\( moduleName, dirAttempts ) -> ( moduleName, isModuleFinished model moduleName ))


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


hasFailed : Model -> Maybe Model
hasFailed model =
    if
        (not <| isFinished model)
            && (isNothing <| haveNotExhaustedAllOptions model.dirAttempts)
    then
        Just model
    else
        Nothing


update : Msg -> Model -> { rsfModel : Model, rsfGoal : Maybe SourceCode, rsfCmd : Cmd Msg }
update msg model =
    case msg of
        ReadElmModuleResult { contents, portScope } ->
            let
                model2 =
                    { model
                        | dirAttempts =
                            model.dirAttempts
                                |> Dict.update
                                    portScope.dir
                                    (updateDirAttempt contents)
                        , sourceCode = contents
                    }

                ( model3, cmds ) =
                    getNextCmds model2

                goal =
                    getGoal model3
            in
                { rsfModel = model3, rsfCmd = Cmd.batch cmds, rsfGoal = goal |> Result.toMaybe }


updateDirAttempt : Maybe String -> Maybe DirAttempt -> Maybe DirAttempt
updateDirAttempt maybeSourceCode maybeDirAttempt =
    maybeDirAttempt
        |> Maybe.map
            (\_ ->
                case maybeSourceCode of
                    Just sourceCode ->
                        DirSuccess

                    Nothing ->
                        DirFail
            )
