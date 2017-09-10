module Helpers exposing (..)

-- import PackageInfo

import String
import List.Extra exposing (uncons)
import Dict exposing (Dict)
import Set


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


removeDuplicates : List comparable -> List comparable
removeDuplicates l =
    l |> Set.fromList |> Set.toList


unsafeDictGet : comparable -> Dict comparable b -> b
unsafeDictGet key dict =
    case Dict.get key dict of
        Just v ->
            v

        Nothing ->
            Debug.crash ("could not find key " ++ toString key)
