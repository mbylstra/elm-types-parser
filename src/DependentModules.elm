module DependentModules exposing (..)

import Dict exposing (Dict)
import FirstPass
import ModuleInfo exposing (getTypeAliases, getUnionTypes, isExternalName, filterByImports, getExternalNamesModuleInfo)
import Types
    exposing
        ( Block
        , ExternalNamesModuleInfo
        , ModuleInfo
        , ModuleToSource
        , UnionDefinition
        , Name
        , Type
        , LocalTypeAliases
        , LocalUnionTypes
        )


-- getModuleInfos :
--     { moduleToSource : ModuleToSource, usedSymbols : List String }
--     -> List ModuleInfo
-- getModuleInfos { moduleToSource, usedSymbols } =
--     let
--         moduleInfos =
--             moduleToSource
--                 |> Dict.values
--                 |> List.map SubjectModuleInfo.getModuleInfo
--     in
--         moduleInfo


getModuleInfo : { sourceCode : String, relevantNames : List String } -> ModuleInfo
getModuleInfo { sourceCode, relevantNames } =
    let
        blocks : List Block
        blocks =
            FirstPass.parseModule sourceCode

        localUnionTypes =
            getUnionTypes blocks

        localTypeAliases =
            getTypeAliases blocks

        directlyReferencedUnionTypes =
            getDirectlyReferencedUnionTypes
                { unionTypes = localUnionTypes, relevantNames = relevantNames }

        directlyReferencedTypeAliases =
            getDirectlyReferencedTypeAliases
                { typeAliases = localTypeAliases, relevantNames = relevantNames }

        -- then for each union type and type alias, get external module names, etc
        definitionNames =
            Dict.keys localTypeAliases ++ Dict.keys localUnionTypes

        externalNames =
            getExternalNames
                { localUnionTypes = localUnionTypes
                , localTypeAliases = localTypeAliases
                }

        imports =
            filterByImports blocks

        reversedImports =
            imports |> List.reverse
    in
        { localUnionTypes = localUnionTypes
        , localTypeAliases = localTypeAliases

        -- , usedTypeNames = usedTypeNames
        , externalNamesModuleInfo = getExternalNamesModuleInfo externalNames imports
        , viewFunctions = Dict.empty -- irrelevant
        }


getDirectlyReferencedUnionTypes :
    { unionTypes : Dict Name UnionDefinition, relevantNames : List String }
    -> Dict Name UnionDefinition
getDirectlyReferencedUnionTypes { unionTypes, relevantNames } =
    unionTypes
        |> Dict.filter (\key value -> List.member key relevantNames)


getDirectlyReferencedTypeAliases :
    { typeAliases : Dict Name Type, relevantNames : List String }
    -> Dict Name Type
getDirectlyReferencedTypeAliases { typeAliases, relevantNames } =
    typeAliases
        |> Dict.filter (\key value -> List.member key relevantNames)


getExternalNames :
    { localTypeAliases : LocalTypeAliases
    , localUnionTypes : LocalUnionTypes
    }
    -> List String
getExternalNames { localTypeAliases, localUnionTypes } =
    let
        namesInTypeAliases =
            Dict.values localTypeAliases |> List.concatMap ModuleInfo.getNames

        namesInUnionTypes =
            Dict.values localUnionTypes |> List.concatMap ModuleInfo.getNamesInUnionDefinition

        allNames =
            namesInTypeAliases ++ namesInUnionTypes

        definitionNames =
            Dict.keys localTypeAliases ++ Dict.keys localUnionTypes
    in
        allNames
            |> List.filter (isExternalName definitionNames)
