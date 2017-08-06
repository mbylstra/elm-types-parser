port module Main exposing (..)

import Process
import Task
import Time exposing (Time)
import PackageInfo exposing (PackageInfo)


{- REMOVE WHEN COMPILER BUG IS FIXED -}

import Json.Decode


port exitApp : Float -> Cmd msg


port externalStop : (() -> msg) -> Sub msg


type alias Flags =
    { elmPackageContents : String
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


init : Flags -> ( Model, Cmd Msg )
init { elmPackageContents } =
    let
        packageInfoResult : Result String PackageInfo
        packageInfoResult =
            Json.Decode.decodeString PackageInfo.decoder elmPackageContents
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


subscriptions : Model -> Sub Msg
subscriptions model =
    externalStop <| always Abort



-- UTILITIES


delayMsg : Time -> Msg -> Cmd Msg
delayMsg time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)
