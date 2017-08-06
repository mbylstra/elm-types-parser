port module Main exposing (..)

import FindFilesToParse exposing (getFilesToParse)
import FirstPass exposing (parseModule)
import Json.Decode
import PackageInfo exposing (PackageInfo)
import Process
import Task
import Time exposing (Time)


{- REMOVE WHEN COMPILER BUG IS FIXED -}

import Json.Decode


port exitApp : Float -> Cmd msg


port externalStop : (() -> msg) -> Sub msg


type alias Flags =
    { elmPackageContents : String
    , viewModuleContents : String
    }


main : Program Flags Model Msg
main =
    Platform.programWithFlags
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { packageInfo : PackageInfo
    }


type Msg
    = Stop
    | Abort
    | ReadElmMessageResult ReadElmModuleResult


type alias ReadElmModuleResult =
    { contents : Maybe String
    , scope : ReadElmModuleScope
    }


init : Flags -> ( Model, Cmd Msg )
init { elmPackageContents, viewModuleContents } =
    let
        packageInfoResult : Result String PackageInfo
        packageInfoResult =
            Json.Decode.decodeString PackageInfo.decoder elmPackageContents

        filesToParse : List String
        filesToParse =
            viewModuleContents
                |> parseModule
                |> getFilesToParse

        _ =
            Debug.log "files to parse" filesToParse
    in
        case packageInfoResult of
            Ok packageInfo ->
                { packageInfo = packageInfo } ! []

            Err err ->
                let
                    err2 =
                        "Invalid elm-package.json.\n\n " ++ err
                in
                    Debug.crash err2


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Stop ->
            model ! [ exitApp 0 ]

        Abort ->
            model ! [ exitApp -1 ]

        ReadElmMessageResult result ->
            model ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    externalStop <| always Abort


type alias ReadElmModuleScope =
    { path : String, dir : String, name : String }


port readElmModule :
    { path : String
    , scope : ReadElmModuleScope
    }
    -> Cmd msg


port readElmModuleResult : (ReadElmModuleResult -> msg) -> Sub msg



-- UTILITIES


delayMsg : Time -> Msg -> Cmd Msg
delayMsg time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)
