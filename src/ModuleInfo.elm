module ModuleInfo exposing (..)

import Types
    exposing
        ( Block(TypeAnnotation, TypeAliasDefinition, Union, Import)
        , Type(Var, Lambda, Tuple, Type, Record)
        , TypeAnnotation
        , TypeAliasDefinition
        , ImportStatement
        , Union
        , UnionDefinition
        , ExternalNamesModuleInfo
        , ModuleInfo
        , LocalTypeAliases
        , LocalUnionTypes
        , Name
        , ViewFunctions
        , DottedModuleName
        )
import Dict exposing (Dict)
import Helpers exposing (groupByFirstTupleItem, anyTrue)
import Set
import ImportStatement
    exposing
        ( isExplicitlyInImportStatement
        )


getTypeAliases : List Block -> Dict Name Type
getTypeAliases blocks =
    blocks
        |> List.filterMap
            (\block ->
                case block of
                    TypeAliasDefinition ( name, tipe ) ->
                        Just ( name, tipe )

                    _ ->
                        Nothing
            )
        |> Dict.fromList


getUnionTypes : List Block -> Dict Name UnionDefinition
getUnionTypes blocks =
    blocks
        |> List.filterMap
            (\block ->
                case block of
                    Union ( name, definition ) ->
                        Just ( name, definition )

                    _ ->
                        Nothing
            )
        |> Dict.fromList


filterByImports : List Block -> List ImportStatement
filterByImports =
    List.filterMap
        (\block ->
            case block of
                Import userImport ->
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
    , "Decoder" -- doesn't work?
    ]


isCoreName : String -> Bool
isCoreName name =
    List.member name coreTypes



-- handleTypeName : LocalNames -> String -> Maybe String
-- handleTypeName localNames typeName =
--     if
--         anyTrue
--             [ List.member typeName coreTypes
--             , List.member typeName localNames
--             ]
--     then
--         Nothing
--     else
--         Just typeName


groupNamesByModule : ExternalNamesModuleInfo -> Dict DottedModuleName (List Name)
groupNamesByModule externalNamesModuleInfo =
    externalNamesModuleInfo
        |> Dict.values
        |> List.map (\{ dottedModulePath, name } -> ( dottedModulePath, name ))
        |> groupByFirstTupleItem


getExternalNamesModuleInfo :
    List String
    -> List ImportStatement
    -> ExternalNamesModuleInfo
getExternalNamesModuleInfo externalNames imports =
    let
        reversedImports =
            imports |> List.reverse
    in
        externalNames
            |> List.concatMap
                (\externalName ->
                    reversedImports
                        |> List.filterMap (isExplicitlyInImportStatement externalName)
                )
            |> Dict.fromList


isExternalName : List Name -> Name -> Bool
isExternalName definitionNames name =
    if String.contains "." name then
        True
    else
        not <| List.member name definitionNames


getNames : Type -> List String
getNames tipe =
    case tipe of
        Var _ ->
            []

        Tuple tipes ->
            List.concatMap getNames tipes

        Lambda leftTipe rightTipe ->
            let
                leftNames =
                    getNames leftTipe

                rightNames =
                    case rightTipe of
                        -- we don't want to include the Html name if it's just the return value.
                        -- However we may want to autogenerate some html for a view function that
                        -- can take Html as an argument.
                        Type "Html" _ ->
                            []

                        _ ->
                            getNames rightTipe
            in
                leftNames ++ rightNames

        Type name tipes ->
            [ name ] ++ (List.concatMap getNames tipes)

        Record fields _ ->
            fields
                |> List.concatMap (Tuple.second >> getNames)


getNamesInUnionDefinition : UnionDefinition -> List String
getNamesInUnionDefinition typeConstructors =
    typeConstructors
        |> List.concatMap (\( _, args ) -> List.concatMap getNames args)


getExternalSymbols : ModuleInfo -> List String
getExternalSymbols info =
    info.externalNamesModuleInfo
        |> Dict.values
        |> List.map .name
        |> Set.fromList
        |> Set.toList


getModulesToLoad : ModuleInfo -> List String
getModulesToLoad info =
    info.externalNamesModuleInfo
        |> Dict.values
        |> List.map .dottedModulePath
        |> Set.fromList
        |> Set.toList
