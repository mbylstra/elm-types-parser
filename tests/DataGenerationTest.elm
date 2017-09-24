module DataGenerationTest exposing (..)

import DataGeneration exposing (lambdaToList)
import Expect exposing (Expectation, equalSets)
import Test exposing (..)
import Types exposing (..)


int : QualifiedType
int =
    QualifiedType { dottedModulePath = "__CORE__", name = "Int" } []


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
        ]
