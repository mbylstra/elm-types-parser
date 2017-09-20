module SubjectModuleInfo exposing (..)

import Types
    exposing
        ( Block(TypeAnnotation, TypeAliasDefinition, Union, Import)
        , Type(Var, Lambda, Tuple, Type, Record)
        , TypeAnnotation
        , TypeAliasDefinition
        , ImportStatement
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
import ViewFunctionDetector exposing (isViewFunction)
import FirstPass
import ModuleInfo
    exposing
        ( LocalTypeDefinitions
        , getTypeAliases
        , getUnionTypes
        , getExternalNamesModuleInfo
        , filterByImports
        , isExternalName
        , isCoreName
        , getNameLocation
        )
import Helpers exposing (removeDuplicates)
import ImportStatement exposing (elmImplicitImports)


getModuleInfo : String -> ModuleInfo
getModuleInfo sourceCode =
    let
        blocks : List Block
        blocks =
            FirstPass.parseModule sourceCode

        localUnionTypes =
            getUnionTypes blocks

        localTypeAliases =
            getTypeAliases blocks

        viewFunctions : Dict Name Type
        viewFunctions =
            getViewFunctions blocks

        externalNames =
            getExternalNames
                { viewFunctions = viewFunctions
                , localUnionTypes = localUnionTypes
                , localTypeAliases = localTypeAliases
                }

        imports =
            elmImplicitImports
                ++ filterByImports blocks

        -- reversedImports =
        --     imports |> List.reverse
    in
        { localUnionTypes = localUnionTypes
        , localTypeAliases = localTypeAliases
        , viewFunctions = viewFunctions
        , externalNamesModuleInfo = getExternalNamesModuleInfo externalNames imports
        }


getExternalNames :
    { viewFunctions : ViewFunctions
    , localTypeAliases : LocalTypeAliases
    , localUnionTypes : LocalUnionTypes
    }
    -> List String
getExternalNames { viewFunctions, localTypeAliases, localUnionTypes } =
    let
        viewFunctionNames =
            viewFunctions
                |> Dict.values
                |> List.concatMap getNames
                |> removeDuplicates
                |> List.filter (not << isCoreName)
    in
        ModuleInfo.getExternalNames (LocalTypeDefinitions localUnionTypes localTypeAliases) viewFunctionNames


getViewFunctions : List Block -> Dict Name Type
getViewFunctions blocks =
    blocks
        |> List.filterMap
            (\block ->
                case block of
                    TypeAnnotation ( name, tipe ) ->
                        if isViewFunction tipe then
                            Just ( name, tipe )
                        else
                            Nothing

                    _ ->
                        Nothing
            )
        |> Dict.fromList


getNames : Type -> List String
getNames mainTipe =
    case mainTipe of
        Type "Html" _ ->
            -- if the view function is just a constant that returns Html (eg: `view : Html msg`),
            -- there's nothing needed to do. We can relax :)
            []

        _ ->
            ModuleInfo.getNames mainTipe
