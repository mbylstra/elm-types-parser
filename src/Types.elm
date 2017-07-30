module Types exposing (..)

{-| Represent Elm types as values! Here are some examples:

    Int            ==> Type "Int" []

    a -> b         ==> Lambda (Var "a") (Var "b")

    ( a, b )       ==> Tuple [ Var "a", Var "b" ]

    Maybe a        ==> Type "Maybe" [ Var "a" ]

    { x : Float }  ==> Record [("x", Type "Float" [])] Nothing

-}


type Type
    = Var String
    | Lambda Type Type
    | Tuple (List Type)
    | Type String (List Type)
    | Record (List ( String, Type )) (Maybe String)
