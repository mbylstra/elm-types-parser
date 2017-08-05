module ImportStatementParser exposing (..)

import Parser exposing (Count(AtLeast), Parser, zeroOrMore, (|.), (|=))
import ElmTypesParser exposing (qualifiedCapVar, whitespace, lowVar, someWhitespace)
import Types exposing (..)


openListing : Listing
openListing =
    Listing [] True


closedListing : Listing
closedListing =
    Listing [] False


listing : List String -> Listing
listing xs =
    Listing xs False


importStatement : Parser UserImport
importStatement =
    Parser.succeed
        (\name maybeAlias exposedNames ->
            ( name, { alias = maybeAlias, exposedNames = exposedNames } )
        )
        |= importStatementName
        |= importAlias
        |= exposedNames


importStatementName : Parser String
importStatementName =
    Parser.succeed identity
        |. Parser.symbol "import"
        |. someWhitespace
        |= qualifiedCapVar


importAlias : Parser (Maybe String)
importAlias =
    Parser.oneOf
        [ Parser.succeed (\name -> Just name)
            |. someWhitespace
            |. Parser.symbol "as"
            |. someWhitespace
            |= qualifiedCapVar
        , Parser.succeed Nothing
        ]


exposedNames : Parser Listing
exposedNames =
    Parser.oneOf
        [ Parser.succeed identity
            |. someWhitespace
            |. Parser.symbol "exposing"
            |. someWhitespace
            |= exposedNamesList
        , Parser.succeed closedListing
        ]


exposedNamesList : Parser Listing
exposedNamesList =
    Parser.oneOf
        [ Parser.symbol "(..)" |> Parser.andThen (\_ -> Parser.succeed openListing)
        , explicitExposedNames |> Parser.andThen (\names -> Parser.succeed (listing names))
        ]


explicitExposedNames : Parser (List String)
explicitExposedNames =
    Parser.succeed (\head tail -> head :: tail)
        |. Parser.symbol "("
        |. whitespace
        |= lowVar
        -- or could be capVar (TODO)
        |. whitespace
        |= Parser.repeat Parser.zeroOrMore
            (Parser.succeed identity
                |. Parser.symbol ","
                |. whitespace
                |= lowVar
                |. whitespace
            )
        |. whitespace
        |. Parser.symbol ")"
