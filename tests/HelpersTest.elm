module HelpersTest exposing (..)

import Expect exposing (Expectation, equalSets)
import Test exposing (..)
import Helpers exposing (qualifiedNameToPath)


suite : Test
suite =
    describe "qualifiedNameToPath"
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
