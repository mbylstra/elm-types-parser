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
generateViewFunction allTypes dottedModulePath ( functionName, functionType ) =
    case functionType of
        QualifiedLambda leftType rightType ->
            let
                argTypes : List QualifiedType
                argTypes =
                    lambdaToList leftType rightType

                args =
                    argTypes
                        |> List.map (generateData allTypes [])
                        |> List.map (\arg -> "(" ++ arg ++ ")")

                qualifiedFunctionName =
                    dottedModulePath ++ "." ++ functionName
            in
                "staticView = " ++ qualifiedFunctionName ++ " " ++ (args |> String.join "")

        _ ->
            Debug.crash "We are only dealing with functions that take args at the moment"


lambdaToList : QualifiedType -> QualifiedType -> List QualifiedType
lambdaToList leftType rightType =
    leftType
        :: (case rightType of
                QualifiedLambda nextLeftType nextRightType ->
                    (lambdaToList nextLeftType nextRightType)

                _ ->
                    [ rightType ]
           )


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

            QualifiedLambda leftType rightType ->
                generateLambda allTypes instantiatedTypeVars leftType rightType

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


generateLambda : QualifiedAllTypes -> InstantiatedTypeVars -> QualifiedType -> QualifiedType -> String
generateLambda allTypes instantiatedTypeVars leftType rightType =
    lambdaToList leftType rightType
        |> List.reverse
        |> (\argsList ->
                case argsList of
                    [] ->
                        Debug.crash "this shouldn't be possible"

                    returnValue :: [] ->
                        Debug.crash "this shouldn't be possible"

                    returnValue :: arg1 :: args ->
                        let
                            numIgnoredArgs =
                                1 + List.length args

                            returnedValue =
                                generateData allTypes instantiatedTypeVars returnValue
                        in
                            "(\\" ++ (List.repeat numIgnoredArgs "_" |> String.join " ") ++ " -> " ++ returnedValue ++ ")"
           )


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
