module ToQualified exposing (..)

import Types exposing (..)
import Dict exposing (Dict)
import ModuleInfo exposing (coreTypes)


qualifyAllTypes : AllTypes -> QualifiedAllTypes
qualifyAllTypes { subjectModuleInfo, allModulesInfo } =
    -- TODO: we must pass in the module name, or just parse the module blah exposing ..
    { subjectModuleInfo = toQualifiedModuleInfo "" subjectModuleInfo
    , allModulesInfo =
        allModulesInfo
            |> Dict.map
                (\dottedModulePath moduleInfo ->
                    toQualifiedModuleInfo dottedModulePath moduleInfo
                )
    }


toQualifiedModuleInfo : DottedModulePath -> ModuleInfo -> QualifiedModuleInfo
toQualifiedModuleInfo dottedModulePath moduleInfo =
    let
        { viewFunctions, localTypeAliases, localUnionTypes, externalNamesModuleInfo } =
            moduleInfo

        qualifiedViewFunctions : Dict Name QualifiedType
        qualifiedViewFunctions =
            viewFunctions |> Dict.map (\name tipe -> qualifyType moduleInfo tipe)

        qualifiedTypeAliases : Dict Name QualifiedType
        qualifiedTypeAliases =
            localTypeAliases |> Dict.map (\name tipe -> qualifyType moduleInfo tipe)

        qualifiedUnionTypes : Dict Name QualifiedUnionR
        qualifiedUnionTypes =
            localUnionTypes |> Dict.map (\name unionR -> qualifyUnionR moduleInfo unionR)
    in
        { dottedModulePath = dottedModulePath
        , viewFunctions = qualifiedViewFunctions
        , typeAliases = qualifiedTypeAliases
        , unionTypes = qualifiedUnionTypes
        }


qualifyUnionR : ModuleInfo -> UnionR -> QualifiedUnionR
qualifyUnionR moduleInfo { name, typeVars, definition } =
    { name = name
    , typeVars = typeVars
    , definition = qualifyUnionDefinition moduleInfo definition
    }


qualifyUnionDefinition : ModuleInfo -> UnionDefinition -> QualifiedUnionDefinition
qualifyUnionDefinition moduleInfo typeConstructors =
    typeConstructors
        |> List.map (qualifyTypeConstructor moduleInfo)


qualifyTypeConstructor : ModuleInfo -> TypeConstructor -> QualifiedTypeConstructor
qualifyTypeConstructor moduleInfo ( name, tipes ) =
    ( name, tipes |> List.map (qualifyType moduleInfo) )


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
                (qualifyName subjectModuleInfo typeName)
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


qualifyName : ModuleInfo -> String -> QualifiedName
qualifyName moduleInfo name =
    case isLocalName moduleInfo name of
        True ->
            QualifiedName moduleInfo.dottedModulePath name

        False ->
            qualifyExternalName moduleInfo.externalNamesModuleInfo name


isLocalName : ModuleInfo -> String -> Bool
isLocalName subjectModuleInfo name =
    case Dict.get name subjectModuleInfo.localUnionTypes of
        Just _ ->
            True

        Nothing ->
            case Dict.get name subjectModuleInfo.localTypeAliases of
                Just _ ->
                    True

                Nothing ->
                    False


qualifyLocalName : String -> String -> QualifiedName
qualifyLocalName dottedModulePath name =
    { dottedModulePath = dottedModulePath
    , name = name
    }


qualifyExternalName : ExternalNamesModuleInfo -> String -> { dottedModulePath : String, name : String }
qualifyExternalName externalNamesModuleInfo name =
    if List.member name coreTypes then
        { dottedModulePath = "__CORE__", name = name }
    else
        case Dict.get name externalNamesModuleInfo of
            Just externalNamesModuleInfo ->
                externalNamesModuleInfo

            Nothing ->
                Debug.crash ("couldnt find " ++ name ++ " in " ++ toString externalNamesModuleInfo)
