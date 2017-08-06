module FindFilesToParse exposing (..)

import Types
    exposing
        ( Block(TypeAnnotation, TypeAliasDefinition, Union)
        , Type(Var, Lambda, Tuple, Type, Record)
        , TypeAnnotation
        , TypeAliasDefinition
        , Union
        )
import Maybe.Extra exposing (unwrap)


getAllFilesToParse : List Block -> List String
getAllFilesToParse blocks =
    let
        localNames =
            getLocalNames blocks
    in
        blocks
            |> filterTypeExpressions
            |> List.concatMap (getFilesToParse localNames)


filterTypeExpressions : List Block -> List Type
filterTypeExpressions =
    List.filterMap
        (\block ->
            case block of
                TypeAnnotation ( _, tipe ) ->
                    Just tipe

                TypeAliasDefinition ( _, tipe ) ->
                    Just tipe

                _ ->
                    Nothing
        )


type alias LocalNames =
    List String


getLocalNames : List Block -> List String
getLocalNames blocks =
    blocks
        |> List.filterMap
            (\block ->
                case block of
                    TypeAliasDefinition ( name, _ ) ->
                        Just name

                    Union ( name, _ ) ->
                        Just name

                    _ ->
                        Nothing
            )


getFilesToParse : LocalNames -> Type -> List String
getFilesToParse localNames tipe =
    case tipe of
        Var _ ->
            []

        Lambda typeA typeB ->
            getFilesToParse localNames typeA
                |> (++) (getFilesToParse localNames typeB)

        Tuple tupleItems ->
            tupleItems
                |> List.concatMap (getFilesToParse localNames)

        Type name concreteTypeVars ->
            handleTypeName localNames name |> unwrap [] List.singleton

        Record fields _ ->
            fields
                |> List.concatMap (Tuple.second >> (getFilesToParse localNames))


coreTypes : List String
coreTypes =
    [ "Bool", "Int", "Float", "String", "Char" ]


anyTrue : List Bool -> Bool
anyTrue =
    List.any identity


handleTypeName : LocalNames -> String -> Maybe String
handleTypeName localNames typeName =
    if
        anyTrue
            [ List.member typeName coreTypes
            , List.member typeName localNames
            ]
    then
        Nothing
    else
        Just typeName
