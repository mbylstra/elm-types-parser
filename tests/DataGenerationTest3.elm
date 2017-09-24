module DataGenerationTest3 exposing (..)

import DataGeneration exposing (generateFromUnionType, generateViewFunctions, generateViewFunction)
import Dict
import Expect exposing (Expectation, equalSets)
import Test exposing (..)
import Types exposing (..)


subjectModuleInfo =
    { localUnionTypes = Dict.empty
    , localTypeAliases = Dict.empty
    , viewFunctions =
        Dict.fromList
            [ ( "dateView"
              , Lambda
                    (Type "Maybe" ([ Type "Foo.Bar" [] ]))
                    (Type "Html" ([ Var "msg" ]))
              )
            ]
    , externalNamesModuleInfo =
        Dict.fromList
            [ ( "Foo.Bar", { dottedModulePath = "Foo", name = "Bar" } )
            , ( "Maybe", { dottedModulePath = "Maybe", name = "Maybe" } )
            , ( "Html", { dottedModulePath = "Html", name = "Html" } )
            ]
    , dottedModulePath = "Main"
    }


moduleInfos =
    Dict.fromList
        [ ( "Maybe"
          , { localUnionTypes =
                Dict.fromList
                    [ ( "Maybe"
                      , { name = "Maybe"
                        , typeVars = [ "a" ]
                        , definition = [ ( "Just", [ Var "a" ] ), ( "Nothing", [] ) ]
                        }
                      )
                    ]
            , localTypeAliases = Dict.fromList []
            , externalNamesModuleInfo = Dict.fromList []
            , viewFunctions = Dict.fromList []
            , dottedModulePath = "Maybe"
            }
          )
        , ( "Foo"
          , { localTypeAliases = Dict.fromList [ ( "Bar", Type "Int" [] ) ]
            , localUnionTypes = Dict.fromList []
            , externalNamesModuleInfo = Dict.empty
            , viewFunctions = Dict.fromList []
            , dottedModulePath = "Foo"
            }
          )
        ]


suite : Test
suite =
    describe "DataGeneration.elm"
        [ describe "generateViewFunction"
            [ test "dateView" <|
                \_ ->
                    (generateViewFunctions
                        { subjectModuleInfo = subjectModuleInfo, allModulesInfo = moduleInfos }
                    )
                        |> Expect.equal
                            [ "Just  ( 1 ) " ]
            ]
        ]
