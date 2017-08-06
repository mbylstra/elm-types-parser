module ImportStatementTest exposing (..)

import ImportStatement exposing (..)
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
        , test "isExplicityInImport top level dir" <|
            \_ ->
                { name = "div", modulePath = [ "Html" ] }
                    |> isExplicitlyInImport
                        ( "Html"
                        , { alias = Nothing, exposedNames = { open = False, explicits = [] } }
                        )
                    |> Expect.equal
                        (Just "Html")
        , test "isExplicityInImport subdir" <|
            \_ ->
                { name = "string", modulePath = [ "Json", "Decode" ] }
                    |> isExplicitlyInImport
                        ( "Json.Decode"
                        , { alias = Nothing, exposedNames = { open = False, explicits = [] } }
                        )
                    |> Expect.equal
                        (Just "Json.Decode")
        , test "isExplicityInImport using alias" <|
            \_ ->
                { name = "string", modulePath = [ "Decode" ] }
                    |> isExplicitlyInImport
                        ( "Json.Decode"
                        , { alias = Just "Decode", exposedNames = { open = False, explicits = [] } }
                        )
                    |> Expect.equal
                        (Just "Json.Decode")
        , test "isExplicityInImport using exposing" <|
            \_ ->
                { name = "string", modulePath = [] }
                    |> isExplicitlyInImport
                        ( "Json.Decode"
                        , { alias = Just "Decode", exposedNames = { open = False, explicits = [ "string" ] } }
                        )
                    |> Expect.equal
                        (Just "Json.Decode")
        ]
