module ImportStatement exposing (..)

import Parser exposing (Count(AtLeast), Parser, zeroOrMore, (|.), (|=))
import ElmTypesParser
    exposing
        ( qualifiedCapVar
        , whitespace
        , lowVar
        , capVar
        , someWhitespace
        )
import Types
    exposing
        ( ImportStatement
        , Listing
        )
import List.Extra


type alias StructuredRawName =
    { name : String
    , modulePath : List String
    }


type alias RawDottedName =
    String


parseImportStatement : String -> Result Parser.Error ImportStatement
parseImportStatement string =
    Parser.run importStatement string


importStatement : Parser ImportStatement
importStatement =
    Parser.succeed
        ImportStatement
        |= importStatementName
        |= importAlias
        |= exposedNames



-- importStatement : Parser ImportStatement
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
        |= exposedType
        |. whitespace
        |= Parser.repeat Parser.zeroOrMore
            (Parser.succeed identity
                |. Parser.symbol ","
                |. whitespace
                |= exposedType
                |. whitespace
            )
        |. whitespace


exposedType : Parser String
exposedType =
    Parser.oneOf [ unionTypeWithAllConstructors, lowVar, capVar ]



-- For now we just ignore the type constructors (BIG TODO!)


unionTypeWithAllConstructors : Parser String
unionTypeWithAllConstructors =
    -- you need to use delayedCommitMap instead of |. and |= if you want
    -- the parser to be used with oneOf (as |. and |= do not backtrack)
    Parser.delayedCommitMap
        (\name _ -> name)
        capVar
        (Parser.symbol "(..)")


lowOrCapVar : Parser String
lowOrCapVar =
    Parser.oneOf [ lowVar, capVar ]


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
-- findModule : List ImportStatement -> QualifiedName -> String
-- findModule imports { name, modulePath } =
--     let
--         reversedImportStatements = List.reverse imports
--         _findModule : ImportStatement ->
--     importMethodn


{-| Convert a string such as Json.Decode.field to a structure such as
{ name = "field"
, modulePath = ["Json", "Decode" ]
}

Note that this does not do any resolution to figure out the full path based
on import statements.
Eg: "field" will just be converted to { name = "field", modulePath = [] } even
if there is import Json.Decode exposing (field)

-}
rawNameToStructured : String -> StructuredRawName
rawNameToStructured rawName =
    rawName
        |> String.split "."
        |> List.reverse
        |> List.Extra.uncons
        |> Maybe.map (\( head, tail ) -> { name = head, modulePath = List.reverse tail })
        |> Maybe.withDefault { name = "", modulePath = [] }


isExplicitlyInImportStatement :
    String
    -> ImportStatement
    -> Maybe ( RawDottedName, { dottedModulePath : String, name : String } )
isExplicitlyInImportStatement rawDottedName { dottedModulePath, maybeAlias, exposedNames } =
    let
        return =
            if rawNameDottedModulePath == dottedModulePath then
                Just
                    ( rawDottedName
                    , { dottedModulePath = dottedModulePath
                      , name = structuredRawName.name
                      }
                    )
            else
                case maybeAlias of
                    Just theAlias ->
                        if theAlias == rawNameDottedModulePath then
                            Just
                                ( rawDottedName
                                , { dottedModulePath = dottedModulePath
                                  , name = structuredRawName.name
                                  }
                                )
                        else
                            handleAliasDoesntMatch

                    Nothing ->
                        handleAliasDoesntMatch

        rawNameDottedModulePath =
            (structuredRawName.modulePath |> toDottedPath)

        structuredRawName =
            rawNameToStructured rawDottedName

        handleAliasDoesntMatch =
            if structuredRawName.modulePath == [] then
                if exposedNames.explicits |> List.member structuredRawName.name then
                    Just
                        ( rawDottedName
                        , { dottedModulePath = dottedModulePath
                          , name = structuredRawName.name
                          }
                        )
                else
                    Nothing
            else
                Nothing
    in
        return


toDottedPath : List String -> String
toDottedPath segments =
    String.join "." segments
