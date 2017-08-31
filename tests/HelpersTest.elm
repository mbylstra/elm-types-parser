module HelpersTest exposing (..)

import Expect exposing (Expectation, equalSets)
import Test exposing (..)
import Helpers exposing (qualifiedNameToPath, groupByFirstTupleItem)
import Dict


suite : Test
suite =
    describe "Helpers.elm"
        [ describe "qualifiedNameToPath"
            [ test "Foo.Bar.Baz" <|
                \_ ->
                    "Foo.Bar.Baz"
                        |> qualifiedNameToPath
                        |> Expect.equal
                            "Foo/Bar/Baz.elm"
            , test "Foo" <|
                \_ ->
                    "Foo"
                        |> qualifiedNameToPath
                        |> Expect.equal
                            "Foo.elm"
            ]
        , describe "groupBy"
            [ test "creates a list of two if two keys are the same" <|
                \_ ->
                    [ ( "a", 1 ), ( "a", 2 ) ]
                        |> groupByFirstTupleItem
                        |> Expect.equal
                            (Dict.fromList [ ( "a", [ 1, 2 ] ) ])
            , test "creates two lists if two keys are different" <|
                \_ ->
                    [ ( "a", 1 ), ( "b", 2 ) ]
                        |> groupByFirstTupleItem
                        |> Expect.equal
                            (Dict.fromList [ ( "a", [ 1 ] ), ( "b", [ 2 ] ) ])
            ]
        ]
