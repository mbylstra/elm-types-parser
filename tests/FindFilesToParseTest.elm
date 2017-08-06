module FindFilesToParseTest exposing (..)

import Expect exposing (Expectation, equalSets)
import Test exposing (..)
import FindFilesToParse exposing (getAllFilesToParse, handleTypeName)
import FirstPass exposing (parseModule)


suite : Test
suite =
    describe "getFilesToParse"
        [ test "very basic" <|
            \_ ->
                ([ "func : ModuleB.Foo"
                 , "func x = 3"
                 ]
                    |> String.join "\n"
                )
                    |> parseModule
                    |> Debug.log "stuff"
                    |> getAllFilesToParse
                    |> Expect.equal
                        [ "ModuleB.Foo" ]
        , test "very basic 2" <|
            \_ ->
                ([ "type alias Alias = ModuleB.Foo"
                 , "func : Alias"
                 , "func x = 3"
                 ]
                    |> String.join "\n"
                )
                    |> parseModule
                    |> Debug.log "stuff"
                    |> getAllFilesToParse
                    |> Expect.equal
                        [ "ModuleB.Foo" ]
        , test "handleTypeName" <|
            \_ ->
                "ModuleB.Foo"
                    |> handleTypeName []
                    |> Expect.equal
                        (Just
                            "ModuleB.Foo"
                        )
        ]
