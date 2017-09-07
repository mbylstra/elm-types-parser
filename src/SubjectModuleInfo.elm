module SubjectModuleInfo exposing (..)

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
import ViewFunctionDetector exposing (isViewFunction)
import FirstPass
import ModuleInfo
    exposing
        ( getTypeAliases
        , getUnionTypes
        , getExternalNamesModuleInfo
        , filterByImports
        , isExternalName
        , isCoreName
        )
import Helpers exposing (removeDuplicates)


-- what is the actual result we want?
-- every view function
---- every type alis or union type in the view function type definition
-- a list of every union type or type definition used by the view functions
-- for each one of these:
----- is it defined locally or in another module? And if another module,
-- are we handling the case where a type alias references a module?
---- I don't think we are!
-- Also, we need to replace aliases with the actual thingo.
-- { model : Info, modulesToLoad : List String }


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

        _ =
            Debug.log "subject viewFunctions" (Dict.keys viewFunctions)

        -- usedTypeNames =
        --     []
        externalNames =
            getExternalNames
                { viewFunctions = viewFunctions
                , localUnionTypes = localUnionTypes
                , localTypeAliases = localTypeAliases
                }

        _ =
            Debug.log "subject external names" externalNames

        -- |> List.map rawNameToQualifiedName
        imports =
            filterByImports blocks

        reversedImports =
            imports |> List.reverse
    in
        { localUnionTypes = localUnionTypes
        , localTypeAliases = localTypeAliases
        , viewFunctions = viewFunctions

        -- , usedTypeNames = usedTypeNames
        , externalNamesModuleInfo = getExternalNamesModuleInfo externalNames imports
        }



-- maybe take all the external names, check if it is in an "explicity import", but also return the fully
-- qualified version of the import, so we can get a unique on that.


getExternalNames :
    { viewFunctions : ViewFunctions
    , localTypeAliases : LocalTypeAliases
    , localUnionTypes : LocalUnionTypes
    }
    -> List String
getExternalNames { viewFunctions, localTypeAliases, localUnionTypes } =
    let
        allNames =
            viewFunctions
                |> Dict.values
                |> List.concatMap getNames
                |> removeDuplicates
                |> List.filter (not << isCoreName)

        _ =
            Debug.log "allNames" allNames

        definitionNames =
            Dict.keys viewFunctions ++ Dict.keys localTypeAliases ++ Dict.keys localUnionTypes
    in
        allNames
            |> List.filter (isExternalName definitionNames)



-- getTypeNameAliases : List String -> Dict String String
-- getTypeNameAliases externalNames =
--     externalNames
--         |> List.concatMap
--             (\qualifiedName ->
--                 reversedImports
--                     |> List.filterMap (isExplicitlyInImportStatement qualifiedName)
--             )
--         |> Set.fromList
--         |> Set.toList


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



-- isExternalName :
--     { viewFunctions : ViewFunctions
--     , localTypeAliases : LocalTypeAliases
--     , localUnionTypes : LocalUnionTypes
--     }
--     -> String
--     -> Bool
-- isExternalName { localUnionTypes, localTypeAliases, viewFunctions } name =
--     if String.contains "." name then
--         True
--     else
--         not <|
--             (Dict.member name localUnionTypes
--                 || Dict.member name localTypeAliases
--                 || Dict.member name viewFunctions
--             )


getNames : Type -> List String
getNames mainTipe =
    case mainTipe of
        Type "Html" _ ->
            -- if the view function is just a constant that returns Html (eg: `view : Html msg`),
            -- there's nothing needed to do. We can relax :)
            []

        _ ->
            ModuleInfo.getNames mainTipe



-- This turned out to be pointless as we don't actually need to substitute anything. Rather
-- we just need to keep a lookup dict.
-- substituteTypes : Info -> Type -> { tipe : Type, pendingNames : List String }
-- substituteTypes model tipe =
--     case tipe of
--         Var _ ->
--             { tipe = tipe, pendingNames = [] }
--
--         Tuple tipes ->
--             let
--                 concatResultsToTuple :
--                     List { tipe : Type, pendingNames : List String }
--                     -> { tipe : Type, pendingNames : List String }
--                 concatResultsToTuple =
--                     List.foldl
--                         (\{ tipe, pendingNames } { accTipes, accPendingNames } ->
--                             { accTipes = tipe :: accTipes
--                             , accPendingNames = pendingNames ++ accPendingNames
--                             }
--                         )
--                         { accTipes = [], accPendingNames = [] }
--                         >> (\{ accTipes, accPendingNames } ->
--                                 { tipe = Tuple accTipes, pendingNames = accPendingNames }
--                            )
--             in
--                 tipes |> List.map (substituteTypes model) |> concatResultsToTuple
--
--         Lambda leftTipe rightTipe ->
--             let
--                 resultsToLambda :
--                     { tipe : Type, pendingNames : List String }
--                     -> { tipe : Type, pendingNames : List String }
--                     -> { tipe : Type, pendingNames : List String }
--                 resultsToLambda leftResult rightResult =
--                     { tipe = Lambda leftResult.tipe rightResult.tipe
--                     , pendingNames = leftResult.pendingNames ++ rightResult.pendingNames
--                     }
--             in
--                 resultsToLambda (substituteTypes model leftTipe) (substituteTypes model rightTipe)
--
--         Record fields instantiatedTypeVars ->
--             let
--                 resultsToLambda :
--                     { tipe : Type, pendingNames : List String }
--                     -> { tipe : Type, pendingNames : List String }
--                     -> { tipe : Type, pendingNames : List String }
--                 resultsToLambda leftResult rightResult =
--                     { tipe = Lambda leftResult.tipe rightResult.tipe
--                     , pendingNames = leftResult.pendingNames ++ rightResult.pendingNames
--                     }
--             in
--                 fields
--                     |> List.map (Tuple.mapSecond <| substituteTypes model)
--                     |> (\items ->
--                             { tipe =
--                                 Record
--                                     (items
--                                         |> List.map
--                                             (\( fieldName, { tipe, pendingNames } ) ->
--                                                 ( fieldName, tipe )
--                                             )
--                                     )
--                                     instantiatedTypeVars
--                             , pendingNames =
--                                 items
--                                     |> List.map (Tuple.second >> .pendingNames)
--                                     |> List.concat
--                             }
--                        )
--             Type name instantiatedTypeVars ->
--                 if
--                 [ name ]
--
-- getAllExternalNames : List Block -> List String
-- getAllExternalNames blocks =
--     let
--         localNames =
--             getLocalNames blocks
--     in
--         blocks
--             |> filterTypeExpressions
--             |> List.concatMap (getExternalNames localNames)
--             |> removeDuplicates
-- getExternalNames : LocalNames -> Type -> List String
-- getExternalNames localNames tipe =
--     case tipe of
--         Var _ ->
--             []
--                 getExternalNames
--                 localNames
--                 typeA
--                 |> (++) (getExternalNames localNames typeB)
--
--         Tuple tupleItems ->
--             tupleItems
--                 |> List.concatMap (getExternalNames localNames)
--
--         Type name concreteTypeVars ->
--             handleTypeName localNames name |> unwrap [] List.singleton
--
--         Record fields _ ->
--             fields
--                 |> List.concatMap (Tuple.second >> (getExternalNames localNames))
