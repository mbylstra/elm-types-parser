module MainTest exposing (..)

import Dict
import Expect exposing (Expectation, equalSets)
import Main exposing (Flags, Msg(ReadSourceFilesMsg), ProgramStage(LoadingTheSubjectsDependentModules), init)
import ReadSourceFiles exposing (DirAttempt(InFlight), readElmModule)
import Test exposing (..)
import Types exposing (Type(Type, Lambda, Var))


suite : Test
suite =
    describe "Main.elm"
        (let
            expectedModel =
                { programStage = LoadingTheSubjectsDependentModules
                , subjectSourceCode = testFlags.subjectSourceCode
                , sourceDirectories = [ "src", "./elm-stuff/packages/elm-lang/core/5.1.1/src" ]
                , readSourceFilesModel =
                    Dict.fromList
                        [ ( "ModuleA"
                          , { sourceCode = Nothing
                            , dirAttempts =
                                Dict.fromList
                                    [ ( "./elm-stuff/packages/elm-lang/core/5.1.1/src", InFlight )
                                    , ( "src", InFlight )
                                    ]
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
            [ test "init model" <|
                \_ ->
                    model
                        |> Expect.equal
                            expectedModel
            , test "init cmd" <|
                \_ ->
                    cmd
                        |> Expect.equal
                            ([ (Cmd.batch
                                    --
                                    -- { type = "node", branches = [{ type = "map", tagger = <function>, tree = { type = "node", branches = [{ type = "leaf",
                                    --  home = "readElmModule", value = { path = "src/ModuleA.elm", portScope = { path = "src/ModuleA.elm", dir = "src", moduleName = "ModuleA" } } },{ type = "leaf", home = "readElmModule", value = { path = "./elm-stuff/packages/elm-lang/core/5.1.1/src/ModuleA.elm", portScope = { path = "./elm-stuff/packages/elm-lang/core/5.1.1/src/ModuleA.elm", dir = "./elm-stuff/packages/elm-lang/core/5.1.1/src", moduleName = "ModuleA" } } }] } }] }
                                    [ readElmModule
                                        { path = "src/ModuleA.elm"
                                        , portScope =
                                            { path = "src/ModuleA.elm"
                                            , dir = "src"
                                            , moduleName = "ModuleA"
                                            }
                                        }
                                    , readElmModule
                                        { path = "./elm-stuff/packages/elm-lang/core/5.1.1/src/ModuleA.elm"
                                        , portScope =
                                            { path = "./elm-stuff/packages/elm-lang/core/5.1.1/src/ModuleA.elm"
                                            , dir = "./elm-stuff/packages/elm-lang/core/5.1.1/src"
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
import ModuleB exposing (Bar)
type alias Bar = Int
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
    "dependencies": {
        "elm-lang/core": "5.1.1 <= v < 6.0.0"
    },
    "elm-version": "0.18.0 <= v < 0.19.0"
}
        """
    , subjectSourceCode = subjectSourceCode
    , exactDependenciesContents = """{
    "elm-lang/core": "5.1.1"
}
    """
    }
