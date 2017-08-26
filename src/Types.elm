module Types exposing (..)


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
