module ImportStatementParserTest exposing (..)

import ElmTypesParser exposing (lowVar, qualifiedCapVar, someWhitespace, whitespace)
import Expect exposing (Expectation, equalSets)
import Parser exposing (Count(AtLeast), Parser, zeroOrMore, (|.), (|=))
import Test exposing (..)


-- import Parser.LanguageKit as Parser
-- import ElmTypesParser exposing (someWhitespace, qualifiedCapVar, whitespace, lowVar)

import ElmTypesParser exposing (qualifiedCapVar, whitespace, lowVar)
import Test exposing (..)
import Expect exposing (Expectation, equalSets)


-- import Expect exposing (Expectation)
-- import Test exposing (..)
-- import Parser
-- examples
-- import Date exposing (Date)
-- import RemoteData exposing (RemoteData(..), WebData)
-- import I18n
-- import RemoteData.Http as Http
-- import Html exposing (..)
-- import Html.Attributes exposing (href, src)
-- import Html.Events exposing (..)
-- import Styles exposing (..)
-- import Types exposing (TacoUpdate(..), Taco, Commit, Stargazer)
-- import Decoders
-- LISTINGS
-- | A listing of values. Something like (a,b,c) or (..) or (a,b,..)


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



-- |> Parser.andThen
--     (\(name, maybeAlias) ->
--
--     )
-- |> Parser.andThen (\name ->


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


suite : Test
suite =
    describe "ImportStatementParser"
        [ test "explicitExposedNames " <|
            \_ ->
                "(varA, varB)"
                    |> Parser.run explicitExposedNames
                    |> Expect.equal
                        (Ok <|
                            [ "varA", "varB" ]
                        )
        , test "exposedNamesList on (..) " <|
            \_ ->
                "(..)"
                    |> Parser.run exposedNamesList
                    |> Expect.equal
                        (Ok <|
                            { open = True, explicits = [] }
                        )
        , test "exposedNamesList (varA, varB) " <|
            \_ ->
                "(varA,varB)"
                    |> Parser.run exposedNamesList
                    |> Expect.equal
                        (Ok <|
                            { open = False, explicits = [ "varA", "varB" ] }
                        )
        , test "importAlias" <|
            \_ ->
                " as Blah"
                    |> Parser.run importAlias
                    |> Expect.equal
                        (Ok <| Just "Blah")
        , test "importAlias (no alias)" <|
            \_ ->
                ""
                    |> Parser.run importAlias
                    |> Expect.equal
                        (Ok <| Nothing)
        , test "importStatementName" <|
            \_ ->
                "import Blah"
                    |> Parser.run importStatementName
                    |> Expect.equal (Ok "Blah")
        , test "importStatement" <|
            \_ ->
                "import Blah as Blaze exposing (varA, varB)"
                    |> Parser.run importStatement
                    |> Expect.equal
                        (Ok <|
                            ( "Blah"
                            , { alias = Just "Blaze"
                              , exposedNames = { explicits = [ "varA", "varB" ], open = False }
                              }
                            )
                        )
        ]
