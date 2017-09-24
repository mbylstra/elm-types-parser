module ToQualifiedTest exposing (..)

import ToQualified exposing (qualifyType)
import Dict
import Expect exposing (Expectation, equalSets)
import Test exposing (..)
import Types exposing (..)


suite : Test
suite =
    describe "DataGeneration.elm"
        [ describe "qualifyType"
            [ test "Maybe a" <|
                \_ ->
                    let
                        moduleInfo =
                            { viewFunctions = Dict.empty
                            , localTypeAliases = Dict.empty
                            , localUnionTypes = Dict.empty
                            , externalNamesModuleInfo =
                                Dict.fromList
                                    [ ( "Maybe", { dottedModulePath = "Maybe", name = "Maybe" } )
                                    ]
                            , dottedModulePath = "Maybe"
                            }
                    in
                        (qualifyType moduleInfo (Type "Maybe" [ Var "a" ]))
                            |> Expect.equal
                                (QualifiedType
                                    { dottedModulePath = "Maybe", name = "Maybe" }
                                    [ QualifiedVar "a" ]
                                )
            , test "Maybe Foo.Bar" <|
                \_ ->
                    let
                        moduleInfo =
                            { viewFunctions = Dict.empty
                            , localTypeAliases = Dict.empty
                            , localUnionTypes = Dict.empty
                            , externalNamesModuleInfo =
                                Dict.fromList
                                    [ ( "Maybe", { dottedModulePath = "Maybe", name = "Maybe" } )
                                    , ( "Foo.Bar", { dottedModulePath = "Foo", name = "Bar" } )
                                    ]
                            , dottedModulePath = "Maybe"
                            }
                    in
                        (qualifyType moduleInfo (Type "Maybe" [ Type "Foo.Bar" [] ]))
                            |> Expect.equal
                                (QualifiedType
                                    { dottedModulePath = "Maybe", name = "Maybe" }
                                    [ QualifiedType { dottedModulePath = "Foo", name = "Bar" } [] ]
                                )
            ]
        ]
