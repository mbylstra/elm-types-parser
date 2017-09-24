module DependentModules exposing (..)

import Dict exposing (Dict)
import FirstPass
import ModuleInfo exposing (LocalTypeDefinitions, getTypeAliases, getUnionTypes, isExternalName, filterByImports, getExternalNamesModuleInfo)
import Types
    exposing
        ( Block
        , ExternalNamesModuleInfo
        , ModuleInfo
        , ModuleToSource
        , ModuleToModuleInfo
        , UnionDefinition
        , Name
        , Type
        , LocalTypeAliases
        )
import ImportStatement exposing (elmImplicitImports)


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

        -- directlyReferencedUnionTypes =
        --     getDirectlyReferencedUnionTypes
        --         { unionTypes = localUnionTypes, relevantNames = relevantNames }
        --
        -- directlyReferencedTypeAliases =
        --     getDirectlyReferencedTypeAliases
        --         { typeAliases = localTypeAliases, relevantNames = relevantNames }
        -- then for each union type and type alias, get external module names, etc
        definitionNames =
            Dict.keys localTypeAliases ++ Dict.keys localUnionTypes

        externalNames =
            ModuleInfo.getExternalNames
                (LocalTypeDefinitions localUnionTypes localTypeAliases)
                relevantNames

        imports =
            elmImplicitImports
                ++ (filterByImports blocks)

        reversedImports =
            imports |> List.reverse
    in
        { localUnionTypes = localUnionTypes
        , localTypeAliases = localTypeAliases
        , externalNamesModuleInfo = getExternalNamesModuleInfo externalNames imports
        , viewFunctions = Dict.empty -- irrelevant
        , dottedModulePath = ModuleInfo.getDottedModulePath blocks
        }



-- getDirectlyReferencedUnionTypes :
--     { unionTypes : Dict Name UnionDefinition, relevantNames : List String }
--     -> Dict Name UnionDefinition
-- getDirectlyReferencedUnionTypes { unionTypes, relevantNames } =
--     unionTypes
--         |> Dict.filter (\key value -> List.member key relevantNames)
--
--
-- getDirectlyReferencedTypeAliases :
--     { typeAliases : Dict Name Type, relevantNames : List String }
--     -> Dict Name Type
-- getDirectlyReferencedTypeAliases { typeAliases, relevantNames } =
--     typeAliases
--         |> Dict.filter (\key value -> List.member key relevantNames)
