module FindModulesToParseTest exposing (..)

import Expect exposing (Expectation, equalSets)
import Test exposing (..)
import FindModulesToParse exposing (getAllExternalNames, handleTypeName, getModulesToParse)
import FirstPass exposing (parseModule)


suite : Test
suite =
    describe "FindFilesToParse"
        [ test "very basic" <|
            \_ ->
                ([ "func : ModuleB.Foo"
                 ]
                    |> String.join "\n"
                )
                    |> parseModule
                    |> getAllExternalNames
                    |> Expect.equal
                        [ "ModuleB.Foo" ]
        , test "very basic 2" <|
            \_ ->
                ([ "type alias Alias = ModuleB.Foo"
                 , "func : Alias"
                 ]
                    |> String.join "\n"
                )
                    |> parseModule
                    |> getAllExternalNames
                    |> Expect.equal
                        [ "ModuleB.Foo" ]
        , test "lambda" <|
            \_ ->
                ([ "type alias Alias = ModuleA.Foo"
                 , "func : Alias -> C.D"
                 ]
                    |> String.join "\n"
                )
                    |> parseModule
                    |> getAllExternalNames
                    |> Expect.equal
                        [ "C.D", "ModuleA.Foo" ]
        , test "handleTypeName" <|
            \_ ->
                "ModuleB.Foo"
                    |> handleTypeName []
                    |> Expect.equal
                        (Just
                            "ModuleB.Foo"
                        )
        , test "getModulesToParse" <|
            \_ ->
                ([ "import A exposing (Bar)"
                 , "import B.C"
                 , "import C.D as D"
                 , "func : B.C.Foo -> Bar -> Int -> D.Baz"
                 ]
                    |> String.join "\n"
                )
                    |> parseModule
                    |> getModulesToParse
                    |> Expect.equal
                        [ "A", "B.C", "C.D" ]
        ]
