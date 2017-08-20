module DeterminePackageLocationsTest exposing (..)

import DeterminePackageLocations exposing (inRange)
import Dict
import Expect exposing (Expectation, equalSets)
import ReadSourceFiles
    exposing
        ( DirAttempt(DirNotAttemptedYet)
        , OverallStatus(HaveNotExhaustedAllOptions)
        , haveNotExhaustedAllOptions
        , moduleStatus
        )
import Test exposing (..)


suite : Test
suite =
    describe "qualifiedNameToPath"
        [ test "HaveNotExhaustedAllOptions" <|
            \_ ->
                let
                    model =
                        [ ( "module1"
                          , { sourceCode = Nothing
                            , dirAttempts = Dict.fromList [ ( "dir1", DirNotAttemptedYet ) ]
                            }
                          )
                        ]
                            |> Dict.fromList
                in
                    moduleStatus model "module1"
                        |> Expect.equal
                            (HaveNotExhaustedAllOptions { nextDirName = "dir1" })
        , test "inRange" <|
            \_ ->
                (inRange
                    { minimum = { major = 1, minor = 0, patch = 0 }
                    , maximum = { major = 2, minor = 0, patch = 0 }
                    }
                    { major = 1, minor = 0, patch = 0 }
                )
                    |> Expect.equal True
        ]
