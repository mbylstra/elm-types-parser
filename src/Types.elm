module Types exposing (..)


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
    ( String, List TypeConstructor )


type alias TypeAliasDefinition =
    ( String, Type )


type alias UserImport =
    ( RawName, ImportMethod )


type alias RawName =
    String


type alias ImportMethod =
    { alias : Maybe String
    , exposedNames : Listing
    }


type alias Listing =
    { explicits : List String
    , open : Bool
    }
