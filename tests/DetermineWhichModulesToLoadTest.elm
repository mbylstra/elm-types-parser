module DetermineWhichModulesToLoadTest exposing (..)

import DetermineWhichModulesToLoad exposing (doIt, getExternalNames, handleTypeName)
import Dict
import Expect exposing (Expectation, equalSets)
import FirstPass exposing (parseModule)
import Test exposing (..)
import Types exposing (Type(Type, Lambda))


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
                , localTypeAliases = Dict.empty
                , localUnionTypes = Dict.empty
                }
                    |> getExternalNames
                    |> Expect.equal
                        []
        , test "an external type alias" <|
            \_ ->
                let
                    viewFunction =
                        ( "view", Lambda (Type "SomeAlias" []) (Type "Html" []) )
                in
                    { viewFunctions = Dict.fromList <| [ viewFunction ]
                    , localTypeAliases = Dict.empty
                    , localUnionTypes = Dict.empty
                    }
                        |> getExternalNames
                        |> Expect.equal
                            [ "SomeAlias" ]
        , test "local type alias" <|
            \_ ->
                let
                    viewFunction =
                        ( "view", Lambda (Type "SomeAlias" []) (Type "Html" []) )

                    typeAlias =
                        ( "SomeAlias", (Type "Int" []) )
                in
                    { viewFunctions = Dict.fromList <| [ viewFunction ]
                    , localTypeAliases = Dict.fromList <| [ typeAlias ]
                    , localUnionTypes = Dict.empty
                    }
                        |> getExternalNames
                        |> Expect.equal
                            []
        , test "local union type" <|
            \_ ->
                let
                    viewFunction =
                        ( "view", Lambda (Type "SomeUnionType" []) (Type "Html" []) )

                    unionType =
                        ( "SomeUnionType"
                        , [ ( "TypeConstructorA", [] )
                          , ( "TypeConstructorB", [] )
                          ]
                        )
                in
                    { viewFunctions = Dict.fromList <| [ viewFunction ]
                    , localTypeAliases = Dict.empty
                    , localUnionTypes = Dict.fromList <| [ unionType ]
                    }
                        |> getExternalNames
                        |> Expect.equal
                            []
        , test "getModulesToParse" <|
            \_ ->
                ([ "import A exposing (Foo)"
                 , "import B.C"
                 , "import C.D as D"
                 , "import E"
                 , "func : Foo -> A.Baz -> B.C.Baz -> D.Qux -> Html Msg"
                 ]
                    |> String.join "\n"
                )
                    |> parseModule
                    |> doIt
                    |> .modulesToLoad
                    |> Expect.equal
                        [ "A", "B.C", "C.D" ]
        ]
