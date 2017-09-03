module Types exposing (..)

import Dict exposing (Dict)


type Block
    = TypeAliasDefinition TypeAliasDefinition
    | Union Union
    | Import ImportStatement
    | TypeAnnotation TypeAnnotation
    | IgnoreBlock


type Type
    = Var String
    | Lambda Type Type
    | Tuple (List Type)
    | Type String (List Type)
    | Record (List ( String, Type )) (Maybe String)


type alias TypeConstructor =
    ( String, TypeConstructorArgs )


type alias TypeConstructorArgs =
    List Type


type alias Union =
    ( String, UnionDefinition )


type alias UnionDefinition =
    List TypeConstructor


type alias TypeAliasDefinition =
    ( String, Type )


type alias ImportStatement =
    { dottedModulePath : String
    , maybeAlias : Maybe String
    , exposedNames : Listing
    }


type alias TypeAnnotation =
    ( String, Type )


type alias FullModulePath =
    String


type alias Listing =
    { explicits : List String
    , open : Bool
    }



-- DetermineWhichModulesToLoad


type alias ModuleInfo =
    { viewFunctions : ViewFunctions
    , localTypeAliases : LocalTypeAliases
    , localUnionTypes : LocalUnionTypes

    -- , usedTypeNames : List ( String, DefinitionLocation )
    -- , importedNameAliases : Dict String String -- eg: "Decode.Decoder" => ("Json.Decode", "Decoder")
    , externalNamesModuleInfo : ExternalNamesModuleInfo

    -- , modulesToLoad : List String
    }


type DefinitionLocation
    = Local
    | External String


type alias LocalUnionTypes =
    Dict Name UnionDefinition


type alias ViewFunctions =
    Dict Name Type


type alias LocalTypeAliases =
    Dict Name Type


type alias ExternalNamesModuleInfo =
    Dict RawDottedName { dottedModulePath : String, name : String }


type alias RawDottedName =
    String


type alias Name =
    String


type alias DottedModuleName =
    String


type alias SourceCode =
    String


type alias ModuleToSource =
    Dict DottedModuleName SourceCode


type alias ModuleToModuleInfo =
    Dict DottedModuleName ModuleInfo
