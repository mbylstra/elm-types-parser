module DataGenerationTest exposing (..)

import DataGeneration exposing (lambdaToList, generateLambda)
import Expect exposing (Expectation, equalSets)
import Test exposing (..)
import Types exposing (..)
import Dict


int : QualifiedType
int =
    QualifiedType { dottedModulePath = "__CORE__", name = "Int" } []


emptyAllTypes : QualifiedAllTypes
emptyAllTypes =
    { subjectModuleInfo = emptySubjectModuleInfo
    , allModulesInfo = Dict.empty
    }


emptySubjectModuleInfo : QualifiedModuleInfo
emptySubjectModuleInfo =
    { viewFunctions = Dict.empty
    , dottedModulePath = ""
    , unionTypes = Dict.empty
    , typeAliases = Dict.empty
    }


suite : Test
suite =
    describe "DataGeneration.elm"
        [ describe "lambdaToList"
            [ test "basic case" <|
                \_ ->
                    lambdaToList int int
                        |> Expect.equal
                            [ int, int ]
            , test "multiple lambdas" <|
                \_ ->
                    lambdaToList int (QualifiedLambda int int)
                        |> Expect.equal
                            [ int, int, int ]
            ]
        , describe "generateLambda"
            [ test "basic case" <|
                \_ ->
                    (generateLambda
                        emptyAllTypes
                        []
                        int
                        int
                    )
                        |> Expect.equal
                            "(\\_ -> 1)"
            , test "more than one arg" <|
                \_ ->
                    (generateLambda
                        emptyAllTypes
                        []
                        int
                        (QualifiedLambda int int)
                    )
                        |> Expect.equal
                            "(\\_ _ -> 1)"
            ]
        ]
