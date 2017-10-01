module ToQualifiedTest exposing (..)

import ToQualified exposing (qualifyType, toQualifiedModuleInfo)
import Dict
import Expect exposing (Expectation, equalSets)
import Test exposing (..)
import Types exposing (..)


suite : Test
suite =
    describe "DataGeneration.elm"
        [ describe "qualifyType"
            -- [ test "Maybe a" <|
            --     \_ ->
            --         let
            --             moduleInfo =
            --                 { viewFunctions = Dict.empty
            --                 , localTypeAliases = Dict.empty
            --                 , localUnionTypes = Dict.empty
            --                 , externalNamesModuleInfo =
            --                     Dict.fromList
            --                         [ ( "Maybe", { dottedModulePath = "Maybe", name = "Maybe" } )
            --                         ]
            --                 , dottedModulePath = "Maybe"
            --                 }
            --         in
            --             (qualifyType { moduleInfo = moduleInfo, alreadyDone = [] } (Type "Maybe" [ Var "a" ]))
            --                 |> Expect.equal
            --                     (QualifiedType
            --                         { dottedModulePath = "Maybe", name = "Maybe" }
            --                         [ QualifiedVar "a" ]
            --                     )
            -- , test "Maybe Foo.Bar" <|
            --     \_ ->
            --         let
            --             moduleInfo =
            --                 { viewFunctions = Dict.empty
            --                 , localTypeAliases = Dict.empty
            --                 , localUnionTypes = Dict.empty
            --                 , externalNamesModuleInfo =
            --                     Dict.fromList
            --                         [ ( "Maybe", { dottedModulePath = "Maybe", name = "Maybe" } )
            --                         , ( "Foo.Bar", { dottedModulePath = "Foo", name = "Bar" } )
            --                         ]
            --                 , dottedModulePath = "Maybe"
            --                 }
            --         in
            --             (qualifyType { moduleInfo = moduleInfo, alreadyDone = [] } (Type "Maybe" [ Type "Foo.Bar" [] ]))
            --                 |> Expect.equal
            --                     (QualifiedType
            --                         { dottedModulePath = "Maybe", name = "Maybe" }
            --                         [ QualifiedType { dottedModulePath = "Foo", name = "Bar" } [] ]
            --                     )
            [ test "recursive union type" <|
                \_ ->
                    let
                        moduleInfo =
                            { viewFunctions = Dict.empty
                            , localTypeAliases = Dict.empty
                            , localUnionTypes =
                                Dict.fromList
                                    [ ( "RecursiveType"
                                      , UnionR "RecursiveType"
                                            []
                                            [ ( "RecurseMe", [ Type "RecursiveType" [] ] )
                                            , ( "StopRecursing", [] )
                                            ]
                                      )
                                    ]
                            , externalNamesModuleInfo =
                                Dict.fromList
                                    []
                            , dottedModulePath = "Foo"
                            }
                    in
                        (toQualifiedModuleInfo moduleInfo)
                            |> Expect.equal
                                { dottedModulePath = "Foo"
                                , viewFunctions = Dict.fromList []
                                , typeAliases = Dict.fromList []
                                , unionTypes =
                                    Dict.fromList
                                        [ ( "RecursiveType"
                                          , { name = "RecursiveType"
                                            , typeVars = []
                                            , definition =
                                                [ ( "RecurseMe"
                                                  , [ QualifiedType
                                                        { dottedModulePath = "Foo"
                                                        , name = "RecursiveType"
                                                        }
                                                        []
                                                    ]
                                                  )
                                                , ( "StopRecursing", [] )
                                                ]
                                            }
                                          )
                                        ]
                                }
            , test "recursive union type 2?" <|
                \_ ->
                    let
                        moduleInfo =
                            { localUnionTypes =
                                Dict.fromList
                                    [ ( "Set"
                                      , { name = "Set"
                                        , typeVars = [ "t" ]
                                        , definition =
                                            [ ( "Set_elm_builtin"
                                                -- The Tuple bit is pretty wierd?? I guess it is just the unit type?
                                                -- If we try to elminate Set, perhaps we'll get more clues as to waht's going on?
                                              , [ Type "Dict.Dict" ([ Var "t", Tuple [] ]) ]
                                              )
                                            ]
                                        }
                                      )
                                    ]
                            , localTypeAliases = Dict.fromList []
                            , externalNamesModuleInfo = Dict.fromList [ ( "Dict.Dict", { dottedModulePath = "Dict", name = "Dict" } ) ]
                            , viewFunctions = Dict.fromList []
                            , dottedModulePath = "Set"
                            }
                    in
                        (qualifyType { moduleInfo = moduleInfo, alreadyDone = [] } (Var "t"))
                            |> Expect.equal
                                (QualifiedVar "t")
            ]
        ]
