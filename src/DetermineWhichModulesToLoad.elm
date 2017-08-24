module DetermineWhichModulesToLoad exposing (..)

import Types
    exposing
        ( Block(TypeAnnotation, TypeAliasDefinition, Union, UserImport)
        , Type(Var, Lambda, Tuple, Type, Record)
        , TypeAnnotation
        , TypeAliasDefinition
        , UserImport
        , Union
        , UnionDefinition
        )
import Set exposing (Set)
import ImportStatement exposing (rawNameToQualifiedName, isExplicitlyInImport)
import Dict exposing (Dict)
import ViewFunctionDetector exposing (isViewFunction)


type alias Name =
    String


type alias Model =
    { viewFunctions : Dict Name Type
    , typeAliases : Dict Name Type
    , unionTypes : Dict Name UnionDefinition
    }


doIt : List Block -> { model : Model, unresolvedModules : List String }
doIt blocks =
    let
        model =
            { unionTypes = getUnionTypes blocks
            , typeAliases = getTypeAliases blocks
            , viewFunctions = getViewFunctions blocks
            }

        externalNames =
            getExternalNames model
                |> List.map rawNameToQualifiedName

        reversedImports =
            filterByImports blocks |> List.reverse
    in
        { model = model
        , unresolvedModules =
            externalNames
                |> List.concatMap
                    (\qualifiedName ->
                        reversedImports
                            |> List.filterMap (isExplicitlyInImport qualifiedName)
                    )
                |> Set.fromList
                |> Set.toList
        }


getExternalNames : Model -> List String
getExternalNames ({ viewFunctions, typeAliases, unionTypes } as model) =
    let
        allNames =
            viewFunctions |> Dict.values |> List.concatMap getNames
    in
        allNames
            |> List.filter (isLocalName model >> not)


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


isLocalName : Model -> String -> Bool
isLocalName model name =
    Dict.member name model.unionTypes
        || Dict.member name model.typeAliases
        || Dict.member name model.viewFunctions


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
-- substituteTypes : Model -> Type -> { tipe : Type, pendingNames : List String }
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
