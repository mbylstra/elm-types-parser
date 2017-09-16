port module DeterminePackageLocations exposing (..)

import Dict exposing (Dict)
import Path.Posix as Path exposing (joinPath)
import Json.Decode exposing (Decoder)


exactDependenciesDecoder : Decoder (Dict String String)
exactDependenciesDecoder =
    Json.Decode.dict Json.Decode.string


exactDependencyToPackagePath : ( String, String ) -> String
exactDependencyToPackagePath ( fullName, version ) =
    case String.split "/" fullName of
        user :: packageName :: [] ->
            joinPath [ ".", "elm-stuff", "packages", user, packageName, version ]

        _ ->
            Debug.crash (fullName ++ " is not a valid packageName")


getPackagePaths : String -> List String
getPackagePaths exactDependenciesContents =
    exactDependenciesContents
        |> Json.Decode.decodeString exactDependenciesDecoder
        |> Result.map (Dict.toList >> List.map exactDependencyToPackagePath)
        |> Result.withDefault []
