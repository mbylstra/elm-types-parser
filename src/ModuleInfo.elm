module ModuleInfo exposing (..)

import Types
    exposing
        ( Block(TypeAnnotation, TypeAliasDefinition, Union, Import)
        , Type(Var, Lambda, Tuple, Type, Record)
        , TypeAnnotation
        , TypeAliasDefinitionR
        , ImportStatement
        , UnionR
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


type alias LocalTypeDefinitions =
    { unionTypes : Dict Name UnionR
    , typeAliases : Dict Name Type
    }


getTypeAliases : List Block -> Dict Name Type
getTypeAliases blocks =
    blocks
        |> List.filterMap
            (\block ->
                case block of
                    TypeAliasDefinition { name, typeVars, definition } ->
                        Just ( name, definition )

                    _ ->
                        Nothing
            )
        |> Dict.fromList


getUnionTypes : List Block -> LocalUnionTypes
getUnionTypes blocks =
    blocks
        |> List.filterMap
            (\block ->
                case block of
                    Union unionR ->
                        Just ( unionR.name, unionR )

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

                TypeAliasDefinition { definition } ->
                    [ definition ]

                Union { definition } ->
                    definition
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
                    TypeAliasDefinition { name } ->
                        Just name

                    Union { name } ->
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
    , "List"
    , "Attribute"
    , "Dict"
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


groupNamesByModule :
    ExternalNamesModuleInfo
    -> List { moduleName : DottedModuleName, relevantNames : List String }
groupNamesByModule externalNamesModuleInfo =
    externalNamesModuleInfo
        |> Dict.values
        |> List.map (\{ dottedModulePath, name } -> ( dottedModulePath, name ))
        |> groupByFirstTupleItem
        |> Dict.toList
        |> List.map (\( a, b ) -> { moduleName = a, relevantNames = b })


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


type LocalOrExternalName
    = LocalUnionType UnionR
    | LocalTypeAlias Type
    | ExternalName


getNameLocation : LocalTypeDefinitions -> Name -> LocalOrExternalName
getNameLocation { unionTypes, typeAliases } name =
    if String.contains "." name then
        ExternalName
    else
        case Dict.get name unionTypes of
            Just unionDefinition ->
                LocalUnionType unionDefinition

            Nothing ->
                case Dict.get name typeAliases of
                    Just tipe ->
                        LocalTypeAlias tipe

                    Nothing ->
                        ExternalName


isExternalName : LocalTypeDefinitions -> Name -> Bool
isExternalName localTypeDefinitions name =
    case getNameLocation localTypeDefinitions name of
        LocalUnionType _ ->
            False

        LocalTypeAlias _ ->
            False

        ExternalName ->
            True


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


getNamesInUnionDefinition : UnionR -> List String
getNamesInUnionDefinition { name, typeVars, definition } =
    definition
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


getExternalNames : LocalTypeDefinitions -> List String -> List String
getExternalNames localTypeDefinitions names =
    getExternalNames_ localTypeDefinitions { namesToLookUp = names, externalNames = [] }
        |> .externalNames
        |> List.filter (not << isCoreName)


type alias ExternalNamesAcc =
    { namesToLookUp : List String, externalNames : List String }


getExternalNames_ : LocalTypeDefinitions -> ExternalNamesAcc -> ExternalNamesAcc
getExternalNames_ localTypeDefinitions acc =
    let
        { externalNames, namesToLookUp } =
            acc

        { unionTypes, typeAliases } =
            localTypeDefinitions
    in
        case namesToLookUp of
            [] ->
                acc

            name :: moreNames ->
                case getNameLocation localTypeDefinitions name of
                    ExternalName ->
                        getExternalNames_
                            localTypeDefinitions
                            { externalNames = name :: externalNames, namesToLookUp = moreNames }

                    LocalUnionType unionDefinition ->
                        let
                            extraNames =
                                getNamesInUnionDefinition unionDefinition
                        in
                            getExternalNames_
                                localTypeDefinitions
                                { acc | namesToLookUp = moreNames ++ extraNames }

                    LocalTypeAlias tipe ->
                        let
                            extraNames =
                                getNames tipe
                        in
                            getExternalNames_
                                localTypeDefinitions
                                { acc | namesToLookUp = moreNames ++ extraNames }
