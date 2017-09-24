module DataGeneration exposing (..)

import Dict exposing (Dict)
import Helpers exposing (unsafeDictGet, unsafeListHead)
import Types exposing (..)
import ToQualified exposing (qualifyAllTypes)


type alias InstantiatedTypeVars =
    List QualifiedType


generateViewFunctions : AllTypes -> List String
generateViewFunctions unqualifiedAllTypes =
    let
        ({ subjectModuleInfo, allModulesInfo } as allTypes) =
            qualifyAllTypes unqualifiedAllTypes
    in
        subjectModuleInfo.viewFunctions
            |> Dict.toList
            |> List.map (generateViewFunction allTypes)


generateViewFunction : QualifiedAllTypes -> ( String, QualifiedType ) -> String
generateViewFunction allTypes ( functionName, functionTipe ) =
    let
        innards =
            generateData allTypes [] functionTipe
    in
        "staticView = " ++ functionName ++ " " ++ innards


generateData : QualifiedAllTypes -> InstantiatedTypeVars -> QualifiedType -> String
generateData ({ subjectModuleInfo, allModulesInfo } as allTypes) instantiatedTypeVars tipe =
    case tipe of
        QualifiedVar varName ->
            let
                instantiatedType =
                    unsafeListHead instantiatedTypeVars
            in
                generateData allTypes instantiatedTypeVars instantiatedType

        QualifiedLambda leftTipe rightTipe ->
            let
                left =
                    generateData allTypes instantiatedTypeVars leftTipe
            in
                case rightTipe of
                    QualifiedLambda _ _ ->
                        left ++ " " ++ (generateData allTypes instantiatedTypeVars rightTipe)

                    _ ->
                        left

        QualifiedTuple tipes ->
            "("
                ++ (tipes |> List.map (generateData allTypes instantiatedTypeVars) |> String.join ", ")
                ++ ")"

        QualifiedType ({ dottedModulePath, name } as qualifiedName) typeArguments ->
            case name of
                "Int" ->
                    "1"

                "String" ->
                    "\"a string\""

                "Bool" ->
                    "True"

                "Float" ->
                    "1.0"

                "Html" ->
                    """(Html.text "hello")"""

                "List" ->
                    let
                        listType =
                            unsafeListHead typeArguments
                    in
                        "[" ++ generateData allTypes instantiatedTypeVars listType ++ "]"

                _ ->
                    let
                        -- typeArguments
                        -- |> List.map qualifyTypeArgument
                        _ =
                            1
                    in
                        substituteType allTypes qualifiedName typeArguments

        QualifiedRecord fields _ ->
            let
                generateFieldData ( name, tipe ) =
                    name ++ " = " ++ (generateData allTypes instantiatedTypeVars tipe)
            in
                "{"
                    ++ (fields
                            |> List.map generateFieldData
                            |> String.join ", "
                       )
                    ++ "}"


generateFromUnionType : QualifiedAllTypes -> InstantiatedTypeVars -> QualifiedUnionR -> String
generateFromUnionType allTypes instantiatedTypeVars { name, typeVars, definition } =
    definition
        |> unsafeListHead
        |> generateFromTypeConstructor allTypes instantiatedTypeVars


generateFromTypeConstructor : QualifiedAllTypes -> InstantiatedTypeVars -> QualifiedTypeConstructor -> String
generateFromTypeConstructor allTypes instantiateTypeVars ( name, args ) =
    let
        generateArg tipe =
            " ( " ++ generateData allTypes instantiateTypeVars tipe ++ " ) "

        argsString =
            List.map generateArg args |> String.join " "
    in
        name ++ " " ++ argsString



-- We need the full type, not just the type name


substituteType : QualifiedAllTypes -> QualifiedName -> InstantiatedTypeVars -> String
substituteType ({ allModulesInfo } as allTypes) ({ dottedModulePath, name } as qualifiedName) instantiatedTypeVars =
    let
        subjectModuleInfo =
            allModulesInfo |> unsafeDictGet "DataGeneration.elm 129" dottedModulePath
    in
        case Dict.get name subjectModuleInfo.typeAliases of
            Just tipe ->
                generateData allTypes instantiatedTypeVars tipe

            Nothing ->
                case Dict.get name subjectModuleInfo.unionTypes of
                    Just unionDefinition ->
                        generateFromUnionType allTypes instantiatedTypeVars unionDefinition

                    Nothing ->
                        Debug.crash ("could not find " ++ toString qualifiedName)
