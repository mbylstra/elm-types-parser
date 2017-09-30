port module ReadSourceFiles exposing (..)

import Dict exposing (Dict)
import Helpers exposing (qualifiedNameToPath)
import Maybe.Extra exposing (isJust, isNothing)
import Types exposing (DottedModulePath, ModuleToSource, SourceCode)


type alias Model =
    { moduleName : String
    , maybeSourceCode : Maybe String
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
    { maybeContents : Maybe String
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


init : { moduleName : String, sourceDirectories : List String } -> ( Model, List (Cmd Msg) )
init { moduleName, sourceDirectories } =
    -- rather than getting cmds, just run an update after initialising it ?
    let
        model =
            { moduleName = moduleName
            , dirAttempts =
                sourceDirectories
                    |> List.map
                        (((,) |> flip) DirNotAttemptedYet)
                    |> Dict.fromList
            , maybeSourceCode = Nothing
            }
    in
        getNextCmds { model = model, maxCmdsReached = False }


{-| If a module has been lying dormant because the max number of commands was reached,
then this can be used to kick it back into action, which should generate a msg which
keeps the app running until it Finished
-}
kickBackIntoAction : Model -> ( Model, List (Cmd Msg) )
kickBackIntoAction model =
    getNextCmds { model = model, maxCmdsReached = False }


getNextCmds : { model : Model, maxCmdsReached : Bool } -> ( Model, List (Cmd Msg) )
getNextCmds { model, maxCmdsReached } =
    let
        { moduleName, maybeSourceCode, dirAttempts } =
            model

        _ =
            Debug.log "numCmdsInFlight " (numCmdsInFlight model)
    in
        if maxCmdsReached || (numCmdsInFlight model >= 1) then
            -- if maxCmdsReached then
            ( model, [] )
        else
            case maybeSourceCode of
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
            (\(( dirName, dirAttempt ) as listItem) ({ dirAttempts, cmds } as acc) ->
                let
                    path : String
                    path =
                        dirName
                            ++ "/"
                            ++ (qualifiedNameToPath moduleName)

                    ( newDirAttempt, newCmds ) =
                        case cmds of
                            [] ->
                                case dirAttempt of
                                    DirNotAttemptedYet ->
                                        ( ( dirName, InFlight )
                                        , [ readElmModule
                                                { path = path
                                                , portScope =
                                                    { path = path
                                                    , dir = dirName
                                                    , moduleName = moduleName
                                                    }
                                                }
                                          ]
                                        )

                                    _ ->
                                        ( listItem, [] )

                            multipleCmds ->
                                -- we only want to omit one cmd at a time, so lets just keep the existing command and let the fold run to the end
                                ( listItem, multipleCmds )
                in
                    { dirAttempts = newDirAttempt :: dirAttempts
                    , cmds = newCmds
                    }
            )
            { dirAttempts = [], cmds = [] }
        |> (\{ dirAttempts, cmds } ->
                ( dirAttempts |> Dict.fromList
                , cmds
                )
           )



-- what is the first string in the dict exactly?
-- its moduleName, modul


getGoal : Model -> Result Model SourceCode
getGoal model =
    if isFinished model then
        Ok (model.maybeSourceCode |> Maybe.withDefault "")
    else
        Err model


subscription : Sub ( DottedModulePath, Msg )
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
    isJust model.maybeSourceCode



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


update : Msg -> Bool -> Model -> { rsfModel : Model, rsfGoal : Maybe SourceCode, rsfCmds : List (Cmd Msg) }
update msg maxCmdsReached model =
    case msg of
        ReadElmModuleResult { maybeContents, portScope } ->
            let
                model2 =
                    { model
                        | dirAttempts =
                            model.dirAttempts
                                |> Dict.update
                                    portScope.dir
                                    (updateDirAttempt maybeContents)
                        , maybeSourceCode = maybeContents
                    }

                ( model3, cmds ) =
                    getNextCmds { model = model2, maxCmdsReached = maxCmdsReached }

                goal =
                    getGoal model3
            in
                { rsfModel = model3, rsfCmds = cmds, rsfGoal = goal |> Result.toMaybe }


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


isInFlight : DirAttempt -> Bool
isInFlight dirAttempt =
    case dirAttempt of
        InFlight ->
            True

        _ ->
            False


numCmdsInFlight : Model -> Int
numCmdsInFlight model =
    model.dirAttempts
        |> Dict.values
        |> List.filter isInFlight
        |> List.length
