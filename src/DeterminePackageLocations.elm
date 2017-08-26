port module DeterminePackageLocations exposing (..)

import Dict exposing (Dict)
import Path.Posix as Path exposing (joinPath)
import Json.Decode exposing (Decoder)


exactDependenciesDecoder : Decoder (Dict String String)
exactDependenciesDecoder =
    Json.Decode.dict Json.Decode.string


exactDependencyToPath : ( String, String ) -> String
exactDependencyToPath ( fullName, version ) =
    case String.split "/" fullName of
        user :: packageName :: [] ->
            joinPath [ ".", "elm-stuff", "packages", user, packageName, version, "src" ]

        _ ->
            Debug.crash (fullName ++ " is not a valid packageName")


doIt : String -> List String
doIt exactDependenciesContents =
    exactDependenciesContents
        |> Json.Decode.decodeString exactDependenciesDecoder
        |> Result.map (Dict.toList >> List.map exactDependencyToPath)
        |> Result.withDefault []
