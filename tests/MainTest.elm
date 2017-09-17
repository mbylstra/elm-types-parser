module MainTest exposing (..)

import Dict
import Expect exposing (Expectation, equalSets)
import Main
    exposing
        ( Flags
        , Msg(ReadSourceFilesMsg)
        , init
        , update
        , EitherModuleInfo(Loaded, NotLoaded)
        , AllModulesInfo
        , updateWithElmPackageInfoContentsResult
        )
import ReadSourceFiles exposing (DirAttempt(InFlight, DirSuccess), readElmModule)
import Test exposing (..)
import Types exposing (Type(Type, Lambda, Var))


suite : Test
suite =
    describe "Main.elm"
        (let
            expectedInitializedModel =
                { packageSourceDirectoriesFound = True
                , subjectSourceCode = testFlags.subjectSourceCode
                , sourceDirectories = [ "src" ]
                , subjectModuleInfo = expectedSubjectModuleInfo
                , allModulesInfo =
                    Dict.fromList
                        [ ( "ModuleA"
                          , { relevantNames = [ "Foo" ]
                            , eitherModuleInfo =
                                NotLoaded
                                    { moduleName = "ModuleA"
                                    , sourceCode = Nothing
                                    , dirAttempts =
                                        Dict.fromList [ ( "src", InFlight ) ]
                                    }
                            }
                          )
                        ]
                }

            expectedSubjectModuleInfo =
                { localUnionTypes = Dict.fromList []
                , localTypeAliases = Dict.fromList []
                , viewFunctions =
                    Dict.fromList
                        [ ( "view"
                          , Lambda (Type "Foo" []) (Type "Html" ([ Var "msg" ]))
                          )
                        ]
                , externalNamesModuleInfo =
                    Dict.fromList
                        [ ( "Foo"
                          , { dottedModulePath = "ModuleA", name = "Foo" }
                          )
                        ]
                }

            ( initialModel, cmd ) =
                init testFlags

            ( model, _ ) =
                initialModel
                    |> updateWithElmPackageInfoContentsResult []
         in
            [ describe "init"
                [ test "init model" <|
                    \_ ->
                        model
                            |> Expect.equal
                                expectedInitializedModel

                -- The argument to ReadSourceFilesMsg is causing a problem
                -- , test "init cmd" <|
                --     \_ ->
                --         cmd
                --             |> Expect.equal
                --                 ([ (Cmd.batch
                --                         [ readElmModule
                --                             { path = "src/ModuleA.elm"
                --                             , portScope =
                --                                 { path = "src/ModuleA.elm"
                --                                 , dir = "src"
                --                                 , moduleName = "ModuleA"
                --                                 }
                --                             }
                --                         ]
                --                    )
                --                     |> Cmd.map (ReadSourceFilesMsg "ModuleA")
                --                  ]
                --                     |> Cmd.batch
                --                 )
                ]
            , describe "update"
                (let
                    msg =
                        ReadSourceFilesMsg
                            "ModuleA"
                            (ReadSourceFiles.ReadElmModuleResult
                                { contents = Just moduleASourceCode
                                , portScope =
                                    { path = "src/ModuleA.elm"
                                    , dir = "src"
                                    , moduleName = "ModuleA"
                                    }
                                }
                            )

                    ( newModel, cmd ) =
                        update msg expectedInitializedModel

                    expectedNewReadSourceFilesModel =
                        Dict.fromList
                            [ ( "ModuleA"
                              , { sourceCode = Just moduleASourceCode
                                , dirAttempts =
                                    Dict.fromList
                                        [ ( "src", DirSuccess )
                                        ]
                                }
                              )
                            ]

                    expectedNewAllModulesInfo =
                        Dict.fromList
                            [ ( "ModuleA"
                              , { relevantNames = [ "Foo" ]
                                , eitherModuleInfo =
                                    Loaded
                                        { localUnionTypes = Dict.fromList []
                                        , localTypeAliases = Dict.fromList [ ( "Foo", Type "Bar" [] ) ]
                                        , externalNamesModuleInfo =
                                            Dict.fromList
                                                [ ( "Bar"
                                                  , { dottedModulePath = "ModuleB", name = "Bar" }
                                                  )
                                                ]
                                        , viewFunctions = Dict.fromList []
                                        }
                                }
                              )
                            , ( "ModuleB"
                              , { relevantNames = [ "Bar" ]
                                , eitherModuleInfo =
                                    NotLoaded
                                        { moduleName = "ModuleB"
                                        , sourceCode = Nothing
                                        , dirAttempts =
                                            Dict.fromList [ ( "src", InFlight ) ]
                                        }
                                }
                              )
                            ]
                 in
                    [ test "expected new AllModulesInfo" <|
                        \_ ->
                            newModel.allModulesInfo
                                |> Expect.equal
                                    expectedNewAllModulesInfo
                    , test "new model" <|
                        \_ ->
                            newModel
                                |> Expect.equal
                                    { expectedInitializedModel
                                        | allModulesInfo = expectedNewAllModulesInfo
                                    }
                    ]
                )
            ]
        )


subjectSourceCode : String
subjectSourceCode =
    """module SomeComponent exposing (..)

import ModuleA exposing (Foo)

view : Foo -> Html msg
view foo =
    text <| toString foo
"""


moduleASourceCode : String
moduleASourceCode =
    """module ModuleA exposing (..)
import ModuleB exposing (Bar)
type alias Foo = Bar
"""


moduleBSourceCode : String
moduleBSourceCode =
    """module ModuleB exposing (..)
import ModuleC exposing (Baz)
type alias Bar = Baz
"""


moduleCSourceCode : String
moduleCSourceCode =
    """module ModuleC exposing (..)
type alias Baz = Int
"""


testFlags : Flags
testFlags =
    { elmPackageContents = """{
    "version": "1.0.0",
    "summary": "helpful summary of your project, less than 80 characters",
    "repository": "https://github.com/user/project.git",
    "license": "BSD3",
    "source-directories": [
        "src"
    ],
    "exposed-modules": [],
    "dependencies": { },
    "elm-version": "0.18.0 <= v < 0.19.0"
}
        """
    , subjectSourceCode = subjectSourceCode
    , exactDependenciesContents = """{}"""
    }
