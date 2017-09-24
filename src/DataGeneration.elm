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
            |> List.map (generateViewFunction allTypes subjectModuleInfo.dottedModulePath)


generateViewFunction : QualifiedAllTypes -> DottedModulePath -> ( String, QualifiedType ) -> String
generateViewFunction allTypes dottedModulePath ( functionName, functionTipe ) =
    let
        _ =
            Debug.log "functionTipe" functionTipe

        innards =
            generateData allTypes [] functionTipe

        qualifiedFunctionName =
            dottedModulePath ++ "." ++ functionName
    in
        "staticView = " ++ qualifiedFunctionName ++ " " ++ innards


generateData : QualifiedAllTypes -> InstantiatedTypeVars -> QualifiedType -> String
generateData ({ subjectModuleInfo, allModulesInfo } as allTypes) instantiatedTypeVars tipe =
    let
        _ =
            Debug.log "instantiatedTypeVars" instantiatedTypeVars
    in
        case tipe of
            QualifiedVar varName ->
                case List.head instantiatedTypeVars of
                    Just instantiatedType ->
                        case instantiatedType of
                            QualifiedVar _ ->
                                "\"String was chosen for wildcard type\""

                            _ ->
                                generateData allTypes instantiatedTypeVars (Debug.log "instantiatedType" instantiatedType)

                    Nothing ->
                        "\"String was chosen for wildcard type\""

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

                    "Date" ->
                        "Date.fromTime 1506233184"

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
    let
        firstConstructor =
            definition
                |> unsafeListHead
    in
        firstConstructor |> generateFromTypeConstructor allTypes instantiatedTypeVars


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
