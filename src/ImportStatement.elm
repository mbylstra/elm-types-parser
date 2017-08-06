module ImportStatement exposing (..)

import Parser exposing (Count(AtLeast), Parser, zeroOrMore, (|.), (|=))
import ElmTypesParser exposing (qualifiedCapVar, whitespace, lowVar, capVar, someWhitespace)
import Types exposing (..)
import List.Extra


parseImportStatement : String -> Result Parser.Error UserImport
parseImportStatement string =
    Parser.run importStatement string


importStatement : Parser UserImport
importStatement =
    Parser.succeed
        (\name maybeAlias exposedNames ->
            ( name, { alias = maybeAlias, exposedNames = exposedNames } )
        )
        |= importStatementName
        |= importAlias
        |= exposedNames



-- importStatement : Parser UserImport
-- importStatement =
--     Parser.succeed
--         (\name exposedNames ->
--             ( name, { alias = Nothing, exposedNames = exposedNames } )
--         )
--         |= importStatementName
--         |= exposedNames


importStatementName : Parser String
importStatementName =
    Parser.succeed identity
        |. Parser.symbol "import"
        |. someWhitespace
        |= qualifiedCapVar


importAlias : Parser (Maybe String)
importAlias =
    let
        hasAlias =
            Parser.delayedCommit someWhitespace <|
                Parser.succeed (\name -> Just name)
                    |. Parser.symbol "as"
                    |. someWhitespace
                    |= qualifiedCapVar

        noAlias =
            Parser.succeed Nothing
    in
        Parser.oneOf
            [ hasAlias
            , noAlias
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
        |= Parser.oneOf [ lowVar, capVar ]
        |. whitespace
        |= Parser.repeat Parser.zeroOrMore
            (Parser.succeed identity
                |. Parser.symbol ","
                |. whitespace
                |= lowVar
                |. whitespace
            )
        |. whitespace


openListing : Listing
openListing =
    Listing [] True


closedListing : Listing
closedListing =
    Listing [] False


listing : List String -> Listing
listing xs =
    Listing xs False



-- Lookup ----------------------------------------------------------------------
-- findModule : List UserImport -> QualifiedName -> String
-- findModule imports { name, modulePath } =
--     let
--         reversedImports = List.reverse imports
--         _findModule : UserImport ->
--     importMethodn


rawNameToQualifiedName : String -> QualifiedName
rawNameToQualifiedName rawName =
    rawName
        |> String.split "."
        |> List.Extra.uncons
        |> Maybe.map (\( head, tail ) -> { name = head, modulePath = tail })
        |> Maybe.withDefault { name = "", modulePath = [] }


modulePathToString : List String -> String
modulePathToString segments =
    String.join "." segments


isExplicitlyInImport : QualifiedName -> UserImport -> Maybe String
isExplicitlyInImport { name, modulePath } ( rawName, { alias, exposedNames } ) =
    let
        modulePathString =
            modulePathToString modulePath
    in
        if modulePathString == rawName then
            Just rawName
        else
            let
                maybeName =
                    case alias of
                        Just theAlias ->
                            if theAlias == modulePathString then
                                Just rawName
                            else
                                Nothing

                        Nothing ->
                            Nothing
            in
                case maybeName of
                    Just theAlias ->
                        Just theAlias

                    Nothing ->
                        if modulePath == [] then
                            if exposedNames.explicits |> List.member name then
                                Just rawName
                            else
                                Nothing
                        else
                            Nothing
