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


allTrue : List Bool -> Bool
allTrue =
    List.all identity


removeDuplicates : List comparable -> List comparable
removeDuplicates l =
    l |> Set.fromList |> Set.toList


unsafeDictGet : String -> comparable -> Dict comparable b -> b
unsafeDictGet errorMessage key dict =
    case Dict.get key dict of
        Just v ->
            v

        Nothing ->
            Debug.crash ("could not find key " ++ toString key ++ " in " ++ errorMessage)


unsafeAssumeSuccess : Result err success -> success
unsafeAssumeSuccess result =
    case result of
        Ok data ->
            data

        Err err ->
            Debug.crash ("Err: " ++ (toString err))


unsafeListHead : List a -> a
unsafeListHead xs =
    case List.head xs of
        Just x ->
            x

        Nothing ->
            Debug.crash "List is empty"
