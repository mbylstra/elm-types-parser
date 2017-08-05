module ImportStatementParserTest exposing (..)

import ImportStatementParser exposing (..)
import Parser
import Expect exposing (Expectation, equalSets)
import Test exposing (..)


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
