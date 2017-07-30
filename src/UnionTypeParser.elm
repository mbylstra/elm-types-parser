-- This is a fork of a lot of the stuff in ElmTypesParser


module Main exposing (..)

-- import Char
-- import Json.Decode as Decode exposing (Decoder)

import Parser exposing (Count(AtLeast), Parser, (|.), (|=))


-- import Parser.LanguageKit as Parser
-- import Set

import Types exposing (..)
import ElmTypesParser exposing (tuple, record, qualifiedCapVar, lowVar, spaces)


tipe : Parser Type
tipe =
    (Parser.lazy <|
        \_ ->
            tipeTerm
                |> Parser.andThen tipeHelp
    )


tipeHelp : Type -> Parser Type
tipeHelp t =
    Parser.oneOf
        [ Parser.map (Lambda t) arrowAndType
        , Parser.succeed t
        ]


arrowAndType : Parser Type
arrowAndType =
    Parser.delayedCommit spaces <|
        Parser.succeed identity
            |. arrow
            |. spaces
            |= tipe


arrow : Parser ()
arrow =
    Parser.symbol "->"


tipeTerm : Parser Type
tipeTerm =
    Parser.lazy <|
        \_ ->
            Parser.oneOf
                [ Parser.map Var lowVar
                , Parser.succeed Type
                    |= qualifiedCapVar
                    |= chompArgs []

                -- This is what we can't do! We have to assume an empty list
                , record
                , tuple
                ]


chompArgs : List Type -> Parser (List Type)
chompArgs revArgs =
    Parser.oneOf
        [ Parser.delayedCommit spaces term
            |> Parser.andThen (\arg -> chompArgs (arg :: revArgs))
        , Parser.succeed (List.reverse revArgs)
        ]


term : Parser Type
term =
    Parser.lazy <|
        \_ ->
            Parser.oneOf
                [ Parser.map Var lowVar
                , Parser.map (flip Type []) qualifiedCapVar
                , record
                , tuple
                ]
