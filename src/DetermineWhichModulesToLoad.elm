module DetermineWhichModulesToLoad exposing (..)

import Types
    exposing
        ( Block(TypeAnnotation, TypeAliasDefinition, Union, Import)
        , Type(Var, Lambda, Tuple, Type, Record)
        , TypeAnnotation
        , TypeAliasDefinition
        , ImportStatement
        , Union
        , UnionDefinition
        )
import Set exposing (Set)


-- import ImportStatement
--     exposing
--         ( toDottedPath
--         , isExplicitlyInImportStatement
--         )

import Dict exposing (Dict)
import ViewFunctionDetector exposing (isViewFunction)


type alias Name =
    String



-- what is the actual result we want?
-- every view function
---- every type alis or union type in the view function type definition
-- a list of every union type or type definition used by the view functions
-- for each one of these:
----- is it defined locally or in another module? And if another module,


type DefinitionLocation
    = Local
    | External String



-- are we handling the case where a type alias references a module?
---- I don't think we are!
-- Also, we need to replace aliases with the actual thingo.


type alias Info =
    { viewFunctions : ViewFunctions
    , localTypeAliases : LocalTypeAliases
    , localUnionTypes : LocalUnionTypes
    , usedTypeNames : List ( String, DefinitionLocation )
    , importedNameAliases : Dict String String -- eg: "Decode.Decoder" => ("Json.Decode", "Decoder")
    , modulesToLoad : List String
    }


type alias LocalUnionTypes =
    Dict Name UnionDefinition


type alias ViewFunctions =
    Dict Name Type


type alias LocalTypeAliases =
    Dict Name Type


doIt : List Block -> Info



-- { model : Info, modulesToLoad : List String }


doIt blocks =
    let
        localUnionTypes =
            getUnionTypes blocks

        localTypeAliases =
            getTypeAliases blocks

        viewFunctions =
            getViewFunctions blocks

        usedTypeNames =
            []

        importedNameAliases =
            Dict.empty

        externalNames =
            getExternalNames { viewFunctions = viewFunctions, localUnionTypes = localUnionTypes, localTypeAliases = localTypeAliases }

        -- |> List.map rawNameToQualifiedName
        imports =
            filterByImports blocks

        reversedImports =
            imports |> List.reverse
    in
        { localUnionTypes = localUnionTypes
        , localTypeAliases = localTypeAliases
        , viewFunctions = viewFunctions
        , usedTypeNames = usedTypeNames
        , importedNameAliases = importedNameAliases
        , modulesToLoad = []

        -- externalNames
        --     |> List.concatMap
        --         (\qualifiedName ->
        --             reversedImports
        --                 |> List.filterMap (isExplicitlyInImportStatement qualifiedName)
        --         )
        --     |> Set.fromList
        --     |> Set.toList
        }



-- maybe take all the external names, check if it is in an "explicity import", but also return the fully
-- qualified version of the import, so we can get a unique on that.


getExternalNames :
    { viewFunctions : ViewFunctions
    , localTypeAliases : LocalTypeAliases
    , localUnionTypes : LocalUnionTypes
    }
    -> List String
getExternalNames ({ viewFunctions, localTypeAliases, localUnionTypes } as defs) =
    let
        allNames =
            viewFunctions |> Dict.values |> List.concatMap getNames
    in
        allNames
            |> List.filter (isExternalName defs)



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


isExternalName :
    { viewFunctions : ViewFunctions
    , localTypeAliases : LocalTypeAliases
    , localUnionTypes : LocalUnionTypes
    }
    -> String
    -> Bool
isExternalName { localUnionTypes, localTypeAliases, viewFunctions } name =
    if String.contains "." name then
        True
    else
        not <|
            (Dict.member name localUnionTypes
                || Dict.member name localTypeAliases
                || Dict.member name viewFunctions
            )


getNames : Type -> List String
getNames mainTipe =
    case mainTipe of
        Type "Html" _ ->
            -- if the view function is just a constant that returns Html (eg: `view : Html msg`),
            -- there's. We can relax :)
            []

        _ ->
            getNamesHelper mainTipe


getNamesHelper : Type -> List String
getNamesHelper tipe =
    case tipe of
        Var _ ->
            []

        Type "Html" _ ->
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

        Type name _ ->
            [ name ]

        Record fields _ ->
            fields
                |> List.concatMap (Tuple.second >> getNames)



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


removeDuplicates : List comparable -> List comparable
removeDuplicates l =
    l |> Set.fromList |> Set.toList


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
