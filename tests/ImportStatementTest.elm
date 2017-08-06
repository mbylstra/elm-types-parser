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
        , test "basic import" <|
            \_ ->
                "import A.B"
                    |> parseImportStatement
                    |> Expect.equal
                        (Ok ( "A.B", { alias = Nothing, exposedNames = { open = False, explicits = [] } } ))
        , test "import with exposing" <|
            \_ ->
                "import A exposing (bar)"
                    |> parseImportStatement
                    |> Expect.equal
                        (Ok ( "A", { alias = Nothing, exposedNames = { open = False, explicits = [ "bar" ] } } ))
        , test "importStatement" <|
            \_ ->
                "import Blah as Blaze exposing (varA, varB)"
                    |> parseImportStatement
                    |> Expect.equal
                        (Ok <|
                            ( "Blah"
                            , { alias = Just "Blaze"
                              , exposedNames = { explicits = [ "varA", "varB" ], open = False }
                              }
                            )
                        )
        , test "importStatement no alias but with exposing" <|
            \_ ->
                "import Blah exposing (VarA, VarB)"
                    |> parseImportStatement
                    |> Expect.equal
                        (Ok <|
                            ( "Blah"
                            , { alias = Nothing
                              , exposedNames = { explicits = [ "VarA", "VarB" ], open = False }
                              }
                            )
                        )
        , test "isExplicityInImport top level dir" <|
            \_ ->
                (isExplicitlyInImport
                    { name = "div", modulePath = [ "Html" ] }
                    ( "Html"
                    , { alias = Nothing, exposedNames = { open = False, explicits = [] } }
                    )
                )
                    |> Expect.equal
                        (Just "Html")
        , test "isExplicityInImport subdir" <|
            \_ ->
                (isExplicitlyInImport
                    { name = "string", modulePath = [ "Json", "Decode" ] }
                    ( "Json.Decode"
                    , { alias = Nothing, exposedNames = { open = False, explicits = [] } }
                    )
                )
                    |> Expect.equal
                        (Just "Json.Decode")
        , test "isExplicityInImport using alias" <|
            \_ ->
                isExplicitlyInImport
                    { name = "string", modulePath = [ "Decode" ] }
                    ( "Json.Decode"
                    , { alias = Just "Decode", exposedNames = { open = False, explicits = [] } }
                    )
                    |> Expect.equal
                        (Just "Json.Decode")
        , test "isExplicityInImport using exposing" <|
            \_ ->
                isExplicitlyInImport
                    { name = "string", modulePath = [] }
                    ( "Json.Decode"
                    , { alias = Just "Decode", exposedNames = { open = False, explicits = [ "string" ] } }
                    )
                    |> Expect.equal
                        (Just "Json.Decode")
        , test "isExplicityInImport B.C.Foo" <|
            \_ ->
                isExplicitlyInImport
                    { name = "Foo", modulePath = [ "B", "C" ] }
                    ( "B.C"
                    , { alias = Nothing, exposedNames = { open = False, explicits = [] } }
                    )
                    |> Expect.equal
                        (Just "B.C")

        -- , test "isExplicityInImport using exposing" <|
        --     \_ ->
        --         isExplicitlyInImport
        --             { name = "string", modulePath = [] }
        --             ( "Json.Decode"
        --             , { alias = Just "Decode", exposedNames = { open = False, explicits = [ "string" ] } }
        --             )
        --             |> Expect.equal
        --                 (Just "Json.Decode")
        ]
