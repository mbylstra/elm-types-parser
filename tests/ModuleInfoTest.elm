module ModuleInfoTest exposing (..)

import Expect exposing (Expectation, equalSets)
import Test exposing (..)
import Types exposing (Type(Type, Lambda), UnionR)
import ModuleInfo exposing (getNames, getExternalNames, getExternalNamesModuleInfo)
import Dict
import FirstPass


suite : Test
suite =
    describe "ModuleInfo"
        [ test "getNames (for Maybe Int)" <|
            \_ ->
                (Type "Maybe" [ Type "Int" [] ])
                    |> getNames
                    |> Expect.equal
                        [ "Maybe", "Int" ]
        , test "getExternalNames" <|
            \_ ->
                let
                    localTypeDefinitions =
                        { unionTypes =
                            Dict.fromList
                                [ ( "Union1"
                                  , UnionR "Union1" [] [ ( "AUnion1Cons1", [ Type "Alias1" [] ] ) ]
                                  )
                                ]
                        , typeAliases =
                            Dict.fromList
                                [ ( "Alias1", Type "ExternalName1" [] ) ]
                        }
                in
                    getExternalNames localTypeDefinitions [ "Union1" ]
                        |> Expect.equal [ "ExternalName1" ]
        , test "getExternalNames2" <|
            \_ ->
                let
                    localTypeDefinitions =
                        { unionTypes =
                            Dict.fromList
                                [ ( "Union1"
                                  , UnionR "Union1" [] [ ( "AUnion1Cons1", [ Type "ExternalName1" [] ] ) ]
                                  )
                                ]
                        , typeAliases =
                            Dict.fromList
                                [ ( "Alias1", Type "Union1" [] ) ]
                        }
                in
                    getExternalNames localTypeDefinitions [ "Alias1" ]
                        |> Expect.equal [ "ExternalName1" ]
        , test "getExternalNames that are not in local definitions" <|
            \_ ->
                let
                    localTypeDefinitions =
                        { unionTypes = Dict.empty
                        , typeAliases = Dict.empty
                        }
                in
                    getExternalNames localTypeDefinitions [ "Foo", "Bar" ]
                        |> Expect.equal [ "Bar", "Foo" ]
        , test "getExternalNamesModuleInfo" <|
            \_ ->
                let
                    importStatements =
                        [ "import A exposing (Foo)"
                        , "import B.C"
                        , "import C.D as D"
                        , "import E"
                        ]
                            |> String.join "\n"
                            |> FirstPass.parseModule
                            |> ModuleInfo.filterByImports
                in
                    getExternalNamesModuleInfo [ "E.apple" ] importStatements
                        |> Expect.equal
                            (Dict.fromList
                                [ ( "E.apple"
                                  , { name = "apple", dottedModulePath = "E" }
                                  )
                                ]
                            )
        , test "getExternalNames for recursive union type" <|
            \_ ->
                let
                    localTypeDefinitions =
                        { unionTypes =
                            Dict.fromList
                                [ ( "RecursiveType"
                                  , UnionR "RecursiveType"
                                        []
                                        [ ( "RecurseMe", [ Type "RecursiveType" [] ] )
                                        , ( "StopRecursing", [] )
                                        ]
                                  )
                                ]
                        , typeAliases = Dict.empty
                        }
                in
                    getExternalNames localTypeDefinitions [ "RecursiveType" ]
                        |> Expect.equal []

        -- (Type "Maybe" [ Type "Int" [] ])
        --     |> getNames
        --     |> Expect.equal
        --         [ "Maybe", "Int" ]
        -- ([ "import A exposing (Foo)"
        --  , "import B.C"
        --  , "import C.D as D"
        --  , "import E"
        --  , "func : Foo -> A.Baz -> B.C.Baz -> D.Qux -> Html Msg"
        ]



-- TODO: test this
-- frame : Bool -> Maybe User -> ActivePage -> Html msg -> Html msg
