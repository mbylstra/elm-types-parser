module Helpers exposing (..)

-- import PackageInfo

import String
import List.Extra exposing (uncons)
import Dict exposing (Dict)


qualifiedNameToPath : String -> String
qualifiedNameToPath name =
    name
        |> String.split "."
        |> List.reverse
        |> uncons
        |> Maybe.map
            (\( fileNameNoExt, reverseBasePath ) ->
                let
                    fileName =
                        fileNameNoExt ++ ".elm"

                    fullReversePath =
                        fileName :: reverseBasePath

                    path =
                        List.reverse fullReversePath
                in
                    String.join "/" path
            )
        -- String.split always returns at least one element, so this can't happen
        |> Maybe.withDefault ""


groupByFirstTupleItem : List ( comparable, a ) -> Dict comparable (List a)
groupByFirstTupleItem rows =
    rows
        |> List.foldr
            (\( key, value ) accDict ->
                accDict
                    |> Dict.update
                        key
                        (\maybeList ->
                            case maybeList of
                                Just list ->
                                    Just <| value :: list

                                Nothing ->
                                    Just <| [ value ]
                        )
            )
            Dict.empty


anyTrue : List Bool -> Bool
anyTrue =
    List.any identity
