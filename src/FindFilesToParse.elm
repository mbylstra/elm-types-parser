module FindFilesToParse exposing (..)

import Types
    exposing
        ( Block(TypeAnnotation, TypeAliasDefinition, Union, UserImport)
        , Type(Var, Lambda, Tuple, Type, Record)
        , TypeAnnotation
        , TypeAliasDefinition
        , UserImport
        , Union
        )
import Maybe.Extra exposing (unwrap)
import Set exposing (Set)
import ImportStatement exposing (rawNameToQualifiedName, isExplicitlyInImport)


getFilesToParse : List Block -> List String
getFilesToParse blocks =
    let
        externalNames =
            getAllExternalNames blocks
                |> List.map rawNameToQualifiedName

        reversedImports =
            filterByImports blocks |> List.reverse

        -- _ =
        --     Debug.log "blocks" blocks
        _ =
            Debug.log "externalNames" externalNames

        _ =
            Debug.log "reversedImports" reversedImports
    in
        externalNames
            |> List.concatMap
                (\qualifiedName ->
                    reversedImports
                        |> List.filterMap (isExplicitlyInImport qualifiedName)
                )
            |> Set.fromList
            |> Set.toList


getAllExternalNames : List Block -> List String
getAllExternalNames blocks =
    let
        localNames =
            getLocalNames blocks
    in
        blocks
            |> filterTypeExpressions
            |> List.concatMap (getExternalNames localNames)
            -- remove duplicates
            |> Set.fromList
            |> Set.toList


filterByImports : List Block -> List UserImport
filterByImports =
    List.filterMap
        (\block ->
            case block of
                UserImport userImport ->
                    Just userImport

                _ ->
                    Nothing
        )


filterTypeExpressions : List Block -> List Type
filterTypeExpressions =
    List.concatMap
        (\block ->
            case block of
                TypeAnnotation ( _, tipe ) ->
                    [ tipe ]

                TypeAliasDefinition ( _, tipe ) ->
                    [ tipe ]

                Union ( _, typeConstructors ) ->
                    typeConstructors
                        |> List.concatMap (Tuple.second)

                _ ->
                    []
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


getExternalNames : LocalNames -> Type -> List String
getExternalNames localNames tipe =
    case tipe of
        Var _ ->
            []

        Lambda typeA typeB ->
            getExternalNames localNames typeA
                |> (++) (getExternalNames localNames typeB)

        Tuple tupleItems ->
            tupleItems
                |> List.concatMap (getExternalNames localNames)

        Type name concreteTypeVars ->
            handleTypeName localNames name |> unwrap [] List.singleton

        Record fields _ ->
            fields
                |> List.concatMap (Tuple.second >> (getExternalNames localNames))


coreTypes : List String
coreTypes =
    [ "Bool"
    , "Int"
    , "Float"
    , "String"
    , "Char"
    , "Html"
    , "List"
    , "Attribute"
    , "Maybe"
    , "Dict"
    , "Result"
    ]


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
