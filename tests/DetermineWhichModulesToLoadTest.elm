module DetermineWhichModulesToLoadTest exposing (..)

import Expect exposing (Expectation, equalSets)
import Test exposing (..)
import DetermineWhichModulesToLoad
    exposing
        ( getExternalNames
        , handleTypeName
        , doIt
        )


-- import FirstPass exposing (parseModule)

import Dict
import Types exposing (Type(Type, Lambda))


-- type alias Model =
--     { viewFunctions : Dict Name Type
--     , typeAliases : Dict Name Type
--     , unionTypes : Dict Name UnionDefinition
--     }


suite : Test
suite =
    describe "FindFilesToParse"
        [ test "most basic" <|
            \_ ->
                { viewFunctions = Dict.fromList <| [ ( "view", Type "Html" [] ) ]
                , typeAliases = Dict.empty
                , unionTypes = Dict.empty
                }
                    |> getExternalNames
                    |> Expect.equal
                        []
        , test "an external alias" <|
            \_ ->
                let
                    viewFunction =
                        ( "view", Lambda (Type "SomeAlias" []) (Type "Html" []) )
                in
                    { viewFunctions = Dict.fromList <| [ viewFunction ]
                    , typeAliases = Dict.empty
                    , unionTypes = Dict.empty
                    }
                        |> getExternalNames
                        |> Expect.equal
                            [ "SomeAlias" ]

        -- , test "lambda" <|
        --     \_ ->
        --         ([ "type alias Alias = ModuleA.Foo"
        --          , "func : Alias -> C.D"
        --          ]
        --             |> String.join "\n"
        --         )
        --             |> parseModule
        --             |> getExternalNames
        --             |> Expect.equal
        --                 [ "C.D", "ModuleA.Foo" ]
        -- , test "handleTypeName" <|
        --     \_ ->
        --         "ModuleB.Foo"
        --             |> handleTypeName []
        --             |> Expect.equal
        --                 (Just
        --                     "ModuleB.Foo"
        --                 )
        -- , test "getModulesToParse" <|
        --     \_ ->
        --         ([ "import A exposing (Bar)"
        --          , "import B.C"
        --          , "import C.D as D"
        --          , "func : B.C.Foo -> Bar -> Int -> D.Baz"
        --          ]
        --             |> String.join "\n"
        --         )
        --             |> parseModule
        --             |> doIt
        --             |> Expect.equal
        --                 [ "A", "B.C", "C.D" ]
        ]
