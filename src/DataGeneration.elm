module DataGeneration exposing (..)

import Dict exposing (Dict)
import Helpers exposing (unsafeDictGet, unsafeListHead)
import Types exposing (..)


type alias ModulesInfo =
    Dict DottedModuleName ModuleInfo


type alias AllTypes =
    { subjectModuleInfo : ModuleInfo
    , allModulesInfo : ModulesInfo
    }


generateViewFunctions : AllTypes -> List String
generateViewFunctions ({ subjectModuleInfo, allModulesInfo } as allTypes) =
    subjectModuleInfo.viewFunctions
        |> Dict.toList
        |> List.map (generateViewFunction allTypes)


generateViewFunction : AllTypes -> ( String, Type ) -> String
generateViewFunction allTypes ( functionName, functionTipe ) =
    let
        innards =
            generateData allTypes functionTipe
    in
        "staticView = " ++ functionName ++ " " ++ innards


generateData : AllTypes -> Type -> String
generateData ({ subjectModuleInfo, allModulesInfo } as allTypes) tipe =
    case tipe of
        Var varName ->
            "TYPEVAR_TODO!"

        Lambda leftTipe rightTipe ->
            let
                left =
                    generateData allTypes leftTipe
            in
                case rightTipe of
                    Lambda _ _ ->
                        left ++ " " ++ (generateData allTypes rightTipe)

                    _ ->
                        left

        Tuple tipes ->
            "("
                ++ (tipes |> List.map (generateData allTypes) |> String.join ", ")
                ++ ")"

        Type typeName typeArguments ->
            case typeName of
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
                        "[" ++ generateData allTypes listType ++ "]"

                _ ->
                    substituteType allTypes typeName

        Record fields _ ->
            let
                generateFieldData ( name, tipe ) =
                    name ++ " = " ++ (generateData allTypes tipe)
            in
                "{"
                    ++ (fields
                            |> List.map generateFieldData
                            |> String.join ", "
                       )
                    ++ "}"


generateFromUnionType : AllTypes -> UnionR -> String
generateFromUnionType allTypes { name, typeVars, definition } =
    definition
        |> unsafeListHead
        |> generateFromTypeConstructor allTypes


generateFromTypeConstructor : AllTypes -> TypeConstructor -> String
generateFromTypeConstructor allTypes ( name, args ) =
    let
        generateArg tipe =
            " ( " ++ generateData allTypes tipe ++ " ) "

        argsString =
            List.map generateArg args |> String.join " "
    in
        name ++ " " ++ argsString


substituteType : AllTypes -> String -> String
substituteType ({ subjectModuleInfo, allModulesInfo } as allTypes) typeName =
    case Dict.get typeName subjectModuleInfo.localTypeAliases of
        Just tipe ->
            generateData allTypes tipe

        Nothing ->
            case Dict.get typeName subjectModuleInfo.localUnionTypes of
                Just unionDefinition ->
                    generateFromUnionType allTypes unionDefinition

                Nothing ->
                    -- unsafeDictGet "DataGeneration.elm line 118" typeName subjectModuleInfo.externalNamesModuleInfo
                    case Dict.get typeName subjectModuleInfo.externalNamesModuleInfo of
                        Just x ->
                            x
                                |> substitueExternalType allTypes.allModulesInfo

                        Nothing ->
                            Debug.crash ("couldnt find " ++ typeName ++ " in " ++ toString allTypes)


substitueExternalType :
    ModulesInfo
    -> { dottedModulePath : String, name : String }
    -> String
substitueExternalType modulesInfo { dottedModulePath, name } =
    let
        moduleInfo =
            unsafeDictGet "DataGeneration.elm line 129" dottedModulePath modulesInfo
    in
        substituteType { subjectModuleInfo = moduleInfo, allModulesInfo = modulesInfo } name
