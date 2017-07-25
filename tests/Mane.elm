module Mane exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import ElmTypesParser exposing (Type(TypeVariable, TypeAlias))
import Parser
import Result.Extra exposing (isErr)


suite : Test
suite =
    describe "ElmTypesParser"
        [ describe "name parser"
            [ test "works" <|
                \_ ->
                    "someFunction"
                        |> Parser.run ElmTypesParser.lowerCamelCaseName
                        |> Expect.equal (Ok "someFunction")
            , test "works on example with digits" <|
                \_ ->
                    "someFunction2"
                        |> Parser.run ElmTypesParser.lowerCamelCaseName
                        |> Expect.equal (Ok "someFunction2")
            , test "fails if starts with digit" <|
                \_ ->
                    "2bad"
                        |> Parser.run ElmTypesParser.lowerCamelCaseName
                        |> isErr
                        |> Expect.equal True
            ]
        , describe "type definition value"
            [ test "works" <|
                \_ ->
                    "a -> b -> Int"
                        |> Parser.run ElmTypesParser.typeDefinitionValue
                        |> Expect.equal
                            (Ok
                                [ TypeVariable "a"
                                , TypeVariable "b"
                                , TypeAlias { name = "Int", typeVariables = [] }
                                ]
                            )
            ]
        , describe "type definition"
            [ test "works" <|
                \_ ->
                    "someFunction : a -> b -> c"
                        |> Parser.run ElmTypesParser.typeDefinition
                        |> Expect.equal
                            (Ok
                                ( "someFunction"
                                , [ TypeVariable "a"
                                  , TypeVariable "b"
                                  , TypeVariable "c"
                                  ]
                                )
                            )
            ]
        ]
