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
    | TypeAlias String


tipe : Parser Type
tipe =
    oneOf [ typeVariable, typeAlias ]


typeVariable : Parser Type
typeVariable =
    succeed TypeVariable
        |= lowerCamelCaseName


typeAlias : Parser Type
typeAlias =
    succeed TypeAlias
        |= upperCamelCaseName


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


rightArrow : Parser ()
rightArrow =
    symbol "->"


lowerCamelCaseName : Parser String
lowerCamelCaseName =
    succeed (++)
        |= keep (Exactly 1) isLower
        |= keep zeroOrMore (\c -> isLetter c || isDigit c)


upperCamelCaseName : Parser String
upperCamelCaseName =
    succeed (++)
        |= keep (Exactly 1) isUpper
        |= keep zeroOrMore (\c -> isLetter c || isDigit c)


spaces : Parser ()
spaces =
    ignore oneOrMore isSpace


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
