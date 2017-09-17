module ImportStatementTest exposing (..)

import ImportStatement
    exposing
        ( isExplicitlyInImportStatement
        , explicitExposedNames
        , exposedNamesList
        , importAlias
        , importStatementName
        , parseImportStatement
        , unionTypeWithAllConstructors
        , exposedType
        )
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
        , test "exposedNamesList for `(..)` " <|
            \_ ->
                "(..)"
                    |> Parser.run exposedNamesList
                    |> Expect.equal
                        (Ok <|
                            { open = True, explicits = [] }
                        )
        , test "exposedNamesList for `(varA, varB)` " <|
            \_ ->
                "(varA,varB)"
                    |> Parser.run exposedNamesList
                    |> Expect.equal
                        (Ok <|
                            { open = False, explicits = [ "varA", "varB" ] }
                        )
        , test "explicitExposedNames for `(varA)` " <|
            \_ ->
                "(varA)"
                    |> Parser.run explicitExposedNames
                    |> Expect.equal (Ok <| [ "varA" ])
        , test "explicitExposedNames for `(varA, varB)` " <|
            \_ ->
                "(varA, varB)"
                    |> Parser.run explicitExposedNames
                    |> Expect.equal (Ok <| [ "varA", "varB" ])
        , test "exposedType for `Foo`" <|
            \_ ->
                "Foo"
                    |> Parser.run exposedType
                    |> Expect.equal (Ok <| "Foo")
        , test "exposedType for `Foo(..)`" <|
            \_ ->
                "Foo"
                    |> Parser.run exposedType
                    |> Expect.equal (Ok <| "Foo")
        , test "explicitExposedNames for `(Foo(..), Bar)` " <|
            \_ ->
                "(Foo(..), Bar)"
                    |> Parser.run explicitExposedNames
                    |> Expect.equal (Ok <| [ "Foo", "Bar" ])
        , test "unionTypeWithAllConstructors for `Foo(..)` " <|
            \_ ->
                "Foo(..)"
                    |> Parser.run unionTypeWithAllConstructors
                    |> Expect.equal (Ok <| "Foo")
        , test "unionTypeWithAllConstructors for `Foo` " <|
            \_ ->
                "Foo"
                    |> Parser.run unionTypeWithAllConstructors
                    |> Expect.err
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
                        (Ok
                            { dottedModulePath = "A.B"
                            , maybeAlias = Nothing
                            , exposedNames = { explicits = [], open = False }
                            }
                        )
        , test "import with exposing" <|
            \_ ->
                "import A exposing (bar)"
                    |> parseImportStatement
                    |> Expect.equal
                        (Ok
                            { dottedModulePath = "A"
                            , maybeAlias = Nothing
                            , exposedNames = { explicits = [ "bar" ], open = False }
                            }
                        )
        , test "importStatement" <|
            \_ ->
                "import Blah as Blaze exposing (varA, varB)"
                    |> parseImportStatement
                    |> Expect.equal
                        (Ok
                            { dottedModulePath = "Blah"
                            , maybeAlias = Just "Blaze"
                            , exposedNames = { explicits = [ "varA", "varB" ], open = False }
                            }
                        )
        , test "importStatement no alias but with exposing" <|
            \_ ->
                "import Blah exposing (VarA, VarB)"
                    |> parseImportStatement
                    |> Expect.equal
                        (Ok
                            { dottedModulePath = "Blah"
                            , maybeAlias = Nothing
                            , exposedNames = { explicits = [ "VarA", "VarB" ], open = False }
                            }
                        )
        , test "isExplicityInImport top level dir" <|
            \_ ->
                (isExplicitlyInImportStatement
                    "Html.div"
                    { dottedModulePath = "Html"
                    , maybeAlias = Nothing
                    , exposedNames = { explicits = [], open = False }
                    }
                )
                    |> Expect.equal
                        (Just
                            ( "Html.div"
                            , { dottedModulePath = "Html"
                              , name = "div"
                              }
                            )
                        )
        , test "isExplicityInImport subdir" <|
            \_ ->
                (isExplicitlyInImportStatement
                    "Json.Decode.string"
                    { dottedModulePath = "Json.Decode"
                    , maybeAlias = Nothing
                    , exposedNames = { explicits = [], open = False }
                    }
                )
                    |> Expect.equal
                        (Just
                            ( "Json.Decode.string"
                            , { dottedModulePath = "Json.Decode"
                              , name = "string"
                              }
                            )
                        )
        , test "isExplicityInImport using alias" <|
            \_ ->
                isExplicitlyInImportStatement
                    "Decode.string"
                    { dottedModulePath = "Json.Decode"
                    , maybeAlias = Just "Decode"
                    , exposedNames = { explicits = [], open = False }
                    }
                    |> Expect.equal
                        (Just
                            ( "Decode.string"
                            , { dottedModulePath = "Json.Decode"
                              , name = "string"
                              }
                            )
                        )
        , test "isExplicityInImport using exposing" <|
            \_ ->
                isExplicitlyInImportStatement
                    "string"
                    { dottedModulePath = "Json.Decode"
                    , maybeAlias = Just "Decode"
                    , exposedNames = { explicits = [ "string" ], open = False }
                    }
                    |> Expect.equal
                        (Just
                            ( "string"
                            , { dottedModulePath = "Json.Decode"
                              , name = "string"
                              }
                            )
                        )
        , test "isExplicityInImport B.C.Foo" <|
            \_ ->
                isExplicitlyInImportStatement
                    "B.C.Foo"
                    { dottedModulePath = "B.C"
                    , maybeAlias = Nothing
                    , exposedNames = { explicits = [], open = False }
                    }
                    |> Expect.equal
                        (Just
                            ( "B.C.Foo"
                            , { dottedModulePath = "B.C"
                              , name = "Foo"
                              }
                            )
                        )
        , test "multiline import statement" <|
            \_ ->
                isExplicitlyInImportStatement
                    "B.C.Foo"
                    { dottedModulePath = "B.C"
                    , maybeAlias = Nothing
                    , exposedNames = { explicits = [], open = False }
                    }
                    |> Expect.equal
                        (Just
                            ( "B.C.Foo"
                            , { dottedModulePath = "B.C"
                              , name = "Foo"
                              }
                            )
                        )

        -- , test "isExplicityInImport using exposing" <|
        --     \_ ->
        --         isExplicitlyInImportStatement
        --             { name = "string", modulePath = [] }
        --             ( "Json.Decode"
        --             , { alias = Just "Decode", exposedNames = { open = False, explicits = [ "string" ] } }
        --             )
        --             |> Expect.equal
        --                 (Just "Json.Decode")
        ]
