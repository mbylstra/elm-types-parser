module ElmTypesParser exposing (..)

-- import Html exposing (..)

import Char exposing (isDigit, isLower, isUpper)
import Parser
    exposing
        ( Count(Exactly)
        , Parser
        , andThen
        , float
        , ignore
        , keep
        , oneOf
        , oneOrMore
        , repeat
        , succeed
        , symbol
        , zeroOrMore
        , (|.)
        , (|=)
        )


type Type
    = TypeVariable String
    | TypeAlias { qualifiedName : List String, typeVariables : List String }


tipe : Parser Type
tipe =
    oneOf [ typeVariable, typeAlias ]


typeVariable : Parser Type
typeVariable =
    succeed TypeVariable
        |= lowerCamelCaseName


typeAlias : Parser Type
typeAlias =
    let
        typeVariables : Parser (List String)
        typeVariables =
            repeat zeroOrMore <|
                succeed identity
                    |. spaces
                    |= lowerCamelCaseName
    in
        succeed
            (\name typeVariables ->
                TypeAlias { qualifiedName = name, typeVariables = typeVariables }
            )
            |= qualifiedTypeName
            |= typeVariables


typeDefinition : Parser ( String, List Type )
typeDefinition =
    succeed (\name args -> ( name, args ))
        |= lowerCamelCaseName
        |. spaces
        |. symbol ":"
        |. spaces
        |= typeDefinitionValue


typeDefinitionValue : Parser (List Type)
typeDefinitionValue =
    let
        typeSignatureArgument : Parser Type
        typeSignatureArgument =
            succeed identity
                |. spaces
                |. rightArrow
                |. spaces
                |= tipe

        extraArguments : Parser (List Type)
        extraArguments =
            repeat zeroOrMore typeSignatureArgument
    in
        succeed (\tipe extraTipes -> tipe :: extraTipes)
            |= tipe
            |= extraArguments


function : Parser (List Type)
function =
    succeed identity
        |. symbol "("
        |. maybeSpaces
        |= typeDefinitionValue
        |. maybeSpaces
        |. symbol ")"


rightArrow : Parser ()
rightArrow =
    symbol "->"


lowerCamelCaseName : Parser String
lowerCamelCaseName =
    succeed (++)
        |= keep (Exactly 1) isLower
        |= keep zeroOrMore (\c -> isLetter c || isDigit c)


qualifiedTypeName : Parser (List String)
qualifiedTypeName =
    let
        subModule =
            succeed identity
                |. symbol "."
                |= upperCamelCaseName
    in
        succeed (::)
            |= upperCamelCaseName
            |= repeat zeroOrMore subModule


upperCamelCaseName : Parser String
upperCamelCaseName =
    succeed (++)
        |= keep (Exactly 1) isUpper
        |= keep zeroOrMore (\c -> isLetter c || isDigit c)


spaces : Parser ()
spaces =
    ignore oneOrMore isSpace


maybeSpaces : Parser ()
maybeSpaces =
    ignore zeroOrMore isSpace


isLetter : Char -> Bool
isLetter c =
    isUpper c || isLower c


isSpace : Char -> Bool
isSpace c =
    c == ' '


testCode : String
testCode =
    """
module Main exposing (..)

import Html exposing (..)

testView : Int -> Html msg
testView i =
    text <| toString i

"""
