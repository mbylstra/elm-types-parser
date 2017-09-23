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


type alias InstantiatedTypeVars =
    List Type


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


qualifyType : ModuleInfo -> Type -> QualifiedType
qualifyType subjectModuleInfo tipe =
    case tipe of
        Var varName ->
            QualifiedVar varName

        Lambda leftTipe rightTipe ->
            QualifiedLambda
                (qualifyType subjectModuleInfo leftTipe)
                (qualifyType subjectModuleInfo rightTipe)

        Tuple tipes ->
            QualifiedTuple (tipes |> List.map (qualifyType subjectModuleInfo))

        Type typeName typeArgs ->
            QualifiedType
                (qualifyExternalName subjectModuleInfo.externalNamesModuleInfo typeName)
                (typeArgs |> List.map (qualifyType subjectModuleInfo))

        Record fields maybeString ->
            let
                qualifiedFields =
                    fields
                        |> List.map
                            (\( name, tipe ) ->
                                ( name
                                , qualifyType subjectModuleInfo tipe
                                )
                            )
            in
                QualifiedRecord qualifiedFields maybeString


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
                    let
                        -- typeArguments
                        -- |> List.map qualifyTypeArgument
                        _ =
                            1
                    in
                        substituteType allTypes typeName typeArguments

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



-- We need the full type, not just the type name


substituteType : AllTypes -> String -> InstantiatedTypeVars -> String
substituteType ({ subjectModuleInfo, allModulesInfo } as allTypes) typeName instantiatedTypeVars =
    case Dict.get typeName subjectModuleInfo.localTypeAliases of
        Just tipe ->
            generateData allTypes tipe

        Nothing ->
            case Dict.get typeName subjectModuleInfo.localUnionTypes of
                Just unionDefinition ->
                    generateFromUnionType allTypes unionDefinition

                Nothing ->
                    -- unsafeDictGet "DataGeneration.elm line 118" typeName subjectModuleInfo.externalNamesModuleInfo
                    let
                        qualifiedExternalName =
                            qualifyExternalName subjectModuleInfo.externalNamesModuleInfo typeName
                    in
                        substituteExternalType allTypes.allModulesInfo instantiatedTypeVars qualifiedExternalName



-- case Dict.get typeName subjectModuleInfo.externalNamesModuleInfo of
--     -- This is where I think we need to "reach in" and get the instantiated type vars
--     Just exteralNamesModuleInfo ->
--         let
--             _ =
--                 Debug.log "substituteType: typeName" typeName
--
--             _ =
--                 Debug.log "substituteType: instantiatedTypeVars" instantiatedTypeVars
--         in
--             substituteExternalType allTypes.allModulesInfo instantiatedTypeVars exteralNamesModuleInfo
--
--     Nothing ->
--         Debug.crash ("couldnt find " ++ typeName ++ " in " ++ toString allTypes)


qualifyExternalName : ExternalNamesModuleInfo -> String -> { dottedModulePath : String, name : String }
qualifyExternalName externalNamesModuleInfo name =
    case Dict.get name externalNamesModuleInfo of
        Just externalNamesModuleInfo ->
            externalNamesModuleInfo

        Nothing ->
            Debug.crash ("couldnt find " ++ name ++ " in " ++ toString externalNamesModuleInfo)


substituteExternalType :
    ModulesInfo
    -> InstantiatedTypeVars
    -> { dottedModulePath : String, name : String }
    -> String
substituteExternalType modulesInfo instantiatedTypeVars { dottedModulePath, name } =
    let
        moduleInfo =
            unsafeDictGet "DataGeneration.elm line 129" dottedModulePath modulesInfo
    in
        substituteType { subjectModuleInfo = moduleInfo, allModulesInfo = modulesInfo } name instantiatedTypeVars
