module MainTest exposing (..)

import Dict
import Expect exposing (Expectation, equalSets)
import Main exposing (Flags, Msg(ReadSourceFilesMsg), ProgramStage(LoadingTheSubjectsDependentModules, LoadingAllDependentModules), init, update)
import ReadSourceFiles exposing (DirAttempt(InFlight, DirSuccess), readElmModule)
import Test exposing (..)
import Types exposing (Type(Type, Lambda, Var))


suite : Test
suite =
    describe "Main.elm"
        (let
            expectedInitializedModel =
                { programStage = LoadingTheSubjectsDependentModules
                , subjectSourceCode = testFlags.subjectSourceCode
                , sourceDirectories = [ "src" ]
                , readSourceFilesModel =
                    Dict.fromList
                        [ ( "ModuleA"
                          , { sourceCode = Nothing
                            , dirAttempts =
                                Dict.fromList [ ( "src", InFlight ) ]
                            }
                          )
                        ]
                , subjectModuleInfo =
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
                }

            ( model, cmd ) =
                init testFlags
         in
            [ describe "init"
                [ test "init model" <|
                    \_ ->
                        model
                            |> Expect.equal
                                expectedInitializedModel
                , test "init cmd" <|
                    \_ ->
                        cmd
                            |> Expect.equal
                                ([ (Cmd.batch
                                        [ readElmModule
                                            { path = "src/ModuleA.elm"
                                            , portScope =
                                                { path = "src/ModuleA.elm"
                                                , dir = "src"
                                                , moduleName = "ModuleA"
                                                }
                                            }
                                        ]
                                   )
                                    |> Cmd.map ReadSourceFilesMsg
                                 ]
                                    |> Cmd.batch
                                )
                ]
            , describe "update"
                (let
                    msg =
                        ReadSourceFilesMsg
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

                    expectedNewProgramStage =
                        LoadingAllDependentModules
                            { moduleInfos =
                                Dict.fromList
                                    [ ( "ModuleA"
                                      , Just
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
                                      )
                                    , ( "ModuleB", Nothing )
                                    ]
                            , readSourceFilesModel =
                                Dict.fromList
                                    [ ( "ModuleB"
                                      , { sourceCode = Nothing
                                        , dirAttempts =
                                            Dict.fromList [ ( "src", InFlight ) ]
                                        }
                                      )
                                    ]
                            }
                 in
                    [ test "new readSoureFilesModel" <|
                        \_ ->
                            newModel.readSourceFilesModel
                                |> Expect.equal
                                    expectedNewReadSourceFilesModel
                    , test "new programStage" <|
                        \_ ->
                            newModel.programStage
                                |> Expect.equal
                                    expectedNewProgramStage

                    -- , test "new model" <|
                    --     \_ ->
                    --         newModel
                    --             |> Expect.equal
                    --                 { expectedInitializedModel
                    --                     | readSourceFilesModel = expectedNewReadSourceFilesModel
                    --                     , programStage = expectedNewProgramStage
                    --                 }
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
