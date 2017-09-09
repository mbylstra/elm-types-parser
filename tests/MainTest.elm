module MainTest exposing (..)

import Dict
import Expect exposing (Expectation, equalSets)
import Main exposing (Flags, ProgramStage(LoadingTheSubjectsDependentModules), init)
import Test exposing (..)


suite : Test
suite =
    describe "Main.elm"
        (let
            expectedModel =
                { programStage = LoadingTheSubjectsDependentModules
                , sourceDirectories =
                    [ "."
                    , "./elm-stuff/packages/elm-lang/core/5.1.1/src"
                    , "./elm-stuff/packages/elm-lang/html/2.0.0/src"
                    , "./elm-stuff/packages/elm-lang/virtual-dom/2.0.4/src"
                    ]
                , readSourceFilesModel = Dict.empty
                , subjectSourceCode = "\n\n    "
                , subjectModuleInfo =
                    { viewFunctions = Dict.empty
                    , localTypeAliases = Dict.empty
                    , localUnionTypes = Dict.empty
                    , externalNamesModuleInfo = Dict.empty
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

            -- , test "init cmd" <|
            --     \_ ->
            --         cmd
            --             |> Expect.equal
            --
            ]
        )


testFlags : Flags
testFlags =
    { elmPackageContents = """
{
    "version": "1.0.0",
    "summary": "helpful summary of your project, less than 80 characters",
    "repository": "https://github.com/user/project.git",
    "license": "BSD3",
    "source-directories": [
        "."
    ],
    "exposed-modules": [],
    "dependencies": {
        "elm-lang/core": "5.1.1 <= v < 6.0.0"
    },
    "elm-version": "0.18.0 <= v < 0.19.0"
}
        """
    , subjectSourceCode = """

    """
    , exactDependenciesContents = """
{
    "elm-lang/virtual-dom": "2.0.4",
    "elm-lang/html": "2.0.0",
    "elm-lang/core": "5.1.1"
}
    """
    }
