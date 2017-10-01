module ToQualified exposing (..)

import Types exposing (..)
import Dict exposing (Dict)
import ModuleInfo exposing (coreTypes)


qualifyAllTypes : AllTypes -> QualifiedAllTypes
qualifyAllTypes { subjectModuleInfo, allModulesInfo } =
    -- TODO: we must pass in the module name, or just parse the module blah exposing ..
    { subjectModuleInfo = toQualifiedModuleInfo subjectModuleInfo
    , allModulesInfo =
        allModulesInfo
            |> Dict.map
                (\dottedModulePath moduleInfo ->
                    if List.member dottedModulePath [ "Dict", "Set" ] then
                        emptyQualifiedModuleInfo
                    else
                        toQualifiedModuleInfo moduleInfo
                )
    }


emptyQualifiedModuleInfo : QualifiedModuleInfo
emptyQualifiedModuleInfo =
    { dottedModulePath = "EMPTY"
    , viewFunctions = Dict.empty
    , typeAliases = Dict.empty
    , unionTypes = Dict.empty
    }


toQualifiedModuleInfo : ModuleInfo -> QualifiedModuleInfo
toQualifiedModuleInfo moduleInfo =
    let
        { viewFunctions, localTypeAliases, localUnionTypes, externalNamesModuleInfo } =
            moduleInfo

        initContext =
            { moduleInfo = moduleInfo, alreadyDone = [] }

        qualifiedViewFunctions : Dict Name QualifiedType
        qualifiedViewFunctions =
            viewFunctions |> Dict.map (\name tipe -> qualifyType initContext tipe)

        qualifiedTypeAliases : Dict Name QualifiedType
        qualifiedTypeAliases =
            localTypeAliases |> Dict.map (\name tipe -> qualifyType initContext tipe)

        qualifiedUnionTypes : Dict Name QualifiedUnionR
        qualifiedUnionTypes =
            localUnionTypes
                |> Dict.map
                    (\name unionR -> qualifyUnionR initContext unionR)
    in
        { dottedModulePath = moduleInfo.dottedModulePath
        , viewFunctions = qualifiedViewFunctions
        , typeAliases = qualifiedTypeAliases
        , unionTypes = qualifiedUnionTypes
        }


qualifyUnionR :
    { moduleInfo : ModuleInfo, alreadyDone : List String }
    -> UnionR
    -> QualifiedUnionR
qualifyUnionR { moduleInfo, alreadyDone } { name, typeVars, definition } =
    { name = name
    , typeVars = typeVars
    , definition =
        qualifyUnionDefinition
            { moduleInfo = moduleInfo, alreadyDone = alreadyDone }
            definition
    }


qualifyUnionDefinition :
    { moduleInfo : ModuleInfo, alreadyDone : List String }
    -> UnionDefinition
    -> QualifiedUnionDefinition
qualifyUnionDefinition { moduleInfo, alreadyDone } typeConstructors =
    typeConstructors
        |> List.map (qualifyTypeConstructor { moduleInfo = moduleInfo, alreadyDone = alreadyDone })


qualifyTypeConstructor :
    { moduleInfo : ModuleInfo, alreadyDone : List String }
    -> TypeConstructor
    -> QualifiedTypeConstructor
qualifyTypeConstructor { moduleInfo, alreadyDone } ( name, tipes ) =
    ( name
    , tipes
        |> List.map
            (qualifyType
                { moduleInfo = moduleInfo, alreadyDone = alreadyDone }
            )
    )


qualifyType :
    { moduleInfo : ModuleInfo, alreadyDone : List String }
    -> Type
    -> QualifiedType
qualifyType ({ moduleInfo, alreadyDone } as context) tipe =
    case tipe of
        Var varName ->
            QualifiedVar varName

        Lambda leftTipe rightTipe ->
            QualifiedLambda
                (qualifyType context leftTipe)
                (qualifyType context rightTipe)

        Tuple tipes ->
            QualifiedTuple (tipes |> List.map (qualifyType context))

        Type typeName typeArgs ->
            QualifiedType
                (qualifyName
                    { moduleInfo = moduleInfo
                    , name = typeName
                    , alreadyDone = alreadyDone
                    }
                )
                (typeArgs |> List.map (qualifyType context))

        Record fields maybeString ->
            let
                qualifiedFields =
                    fields
                        |> List.map
                            (\( name, tipe ) ->
                                ( name
                                , qualifyType context tipe
                                )
                            )
            in
                QualifiedRecord qualifiedFields maybeString


qualifyName :
    { moduleInfo : ModuleInfo, name : String, alreadyDone : List String }
    -> QualifiedName
qualifyName { moduleInfo, name, alreadyDone } =
    case isLocalName moduleInfo name of
        True ->
            QualifiedName moduleInfo.dottedModulePath name

        False ->
            case isExternalName moduleInfo name of
                True ->
                    qualifyExternalName
                        { externalNamesModuleInfo = moduleInfo.externalNamesModuleInfo
                        , name = name
                        , alreadyDone = alreadyDone
                        }

                False ->
                    -- This is bogus, but it won't be used.
                    { dottedModulePath = "IGNORED", name = name }


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


isExternalName : ModuleInfo -> String -> Bool
isExternalName subjectModuleInfo name =
    case Dict.get name subjectModuleInfo.externalNamesModuleInfo of
        Just _ ->
            True

        Nothing ->
            False


{-| The problem is that we parse ALL local type aliases and union types, so this
is a problem if we have one that isn't used by a view function, and references
an external module that we haven't loaded.
-}
isNameToIgnore : ModuleInfo -> String -> Bool
isNameToIgnore subjectModuleInfo name =
    (not <| isLocalName subjectModuleInfo name)
        && (not <| isExternalName subjectModuleInfo name)


qualifyLocalName : String -> String -> QualifiedName
qualifyLocalName dottedModulePath name =
    { dottedModulePath = dottedModulePath
    , name = name
    }


qualifyExternalName :
    { externalNamesModuleInfo : ExternalNamesModuleInfo
    , name : String
    , alreadyDone : List String
    }
    -> { dottedModulePath : String, name : String }
qualifyExternalName { externalNamesModuleInfo, name, alreadyDone } =
    if List.member name coreTypes then
        { dottedModulePath = "__CORE__", name = name }
    else
        case Dict.get name externalNamesModuleInfo of
            Just externalNamesModuleInfo ->
                externalNamesModuleInfo

            Nothing ->
                Debug.crash
                    ("couldnt find "
                        ++ name
                        ++ " in "
                        ++ toString externalNamesModuleInfo
                    )
