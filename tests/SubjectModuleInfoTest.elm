module SubjectModuleInfoTest exposing (..)

import SubjectModuleInfo exposing (getExternalNames)
import Dict
import Expect exposing (Expectation, equalSets)
import Test exposing (..)
import Types exposing (Type(Type, Lambda))
import Dict
import Types exposing (Type(Type, Lambda))


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
                    |> SubjectModuleInfo.getModuleInfo
                    |> .externalNamesModuleInfo
                    |> Expect.equal
                        (Dict.fromList <|
                            [ ( "Foo", { dottedModulePath = "A", name = "Foo" } )
                            , ( "A.Baz", { dottedModulePath = "A", name = "Baz" } )
                            , ( "B.C.Baz", { dottedModulePath = "B.C", name = "Baz" } )
                            , ( "D.Qux", { dottedModulePath = "C.D", name = "Qux" } )
                            ]
                        )
        ]
