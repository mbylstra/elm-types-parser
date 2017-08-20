module DeterminePackageLocationsTest exposing (..)

import DeterminePackageLocations exposing (inRange)
import Expect exposing (Expectation, equalSets)
import Test exposing (..)


suite : Test
suite =
    describe "qualifiedNameToPath"
        [ test "inRange" <|
            \_ ->
                (inRange
                    { minimum = { major = 1, minor = 0, patch = 0 }
                    , maximum = { major = 2, minor = 0, patch = 0 }
                    }
                    { major = 1, minor = 0, patch = 0 }
                )
                    |> Expect.equal True
        ]
