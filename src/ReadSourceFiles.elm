port module ReadSourceFiles exposing (..)

import Dict exposing (Dict)
import Helpers exposing (qualifiedNameToPath)


type alias Model =
    Dict ModuleName (List { dirName : String, status : DirStatus })


type alias ModuleName =
    String


type DirStatus
    = DirNotOpenedYet
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


init : List String -> List String -> ( Model, Cmd Msg )
init moduleNames dirNames =
    let
        model =
            (moduleNames
                |> List.map
                    (\moduleName ->
                        ( moduleName
                        , dirNames
                            |> List.map
                                (\dirName ->
                                    { dirName = dirName, status = DirNotOpenedYet }
                                )
                        )
                    )
                |> Dict.fromList
            )
    in
        model ! [ getNextCmd model ]


getNextCmd : Model -> Cmd msg
getNextCmd model =
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
                            readElmModule
                                { path = path
                                , scope =
                                    { path = path
                                    , dir = nextDirName
                                    , moduleName = moduleName
                                    }
                                }

                    _ ->
                        Cmd.none
            )
        -- |> Debug.log "cmds"
        |> Cmd.batch


subscriptions : Sub Msg
subscriptions =
    readElmModuleResult ReadElmModuleResult


moduleStatus : Model -> ModuleName -> OverallStatus
moduleStatus model moduleName =
    let
        dirStatuses =
            Dict.get moduleName model |> Maybe.withDefault []
    in
        if dirStatuses == [] then
            TotalFail
        else
            case atLeastOneSuccess dirStatuses of
                Just dirName ->
                    Success { dirName = dirName }

                Nothing ->
                    case haveNotExhaustedAllOptions dirStatuses of
                        Just nextDirName ->
                            HaveNotExhaustedAllOptions { nextDirName = nextDirName }

                        Nothing ->
                            TotalFail


moduleStatuses : Model -> List ( String, OverallStatus )
moduleStatuses model =
    model
        |> Dict.toList
        |> List.map
            (\( moduleName, dirStatuses ) -> ( moduleName, moduleStatus model moduleName ))


atLeastOneSuccess : List { dirName : String, status : DirStatus } -> Maybe String
atLeastOneSuccess dirStatuses =
    dirStatuses
        |> List.map
            (\item ->
                case item.status of
                    DirSuccess ->
                        Just item.dirName

                    _ ->
                        Nothing
            )
        |> List.filterMap identity
        |> List.head


haveNotExhaustedAllOptions :
    List { dirName : String, status : DirStatus }
    -> Maybe String
haveNotExhaustedAllOptions dirStatuses =
    dirStatuses
        |> List.map
            (\item ->
                case item.status of
                    DirNotOpenedYet ->
                        Just item.dirName

                    _ ->
                        Nothing
            )
        |> List.filterMap identity
        |> List.head


update : Msg -> Model -> Model
update msg model =
    case msg of
        ReadElmModuleResult readElmModuleResultR ->
            let
                _ =
                    Debug.log "" readElmModuleResultR
            in
                model
