module ReadSourceFilesTest exposing (..)

import ReadSourceFiles exposing (..)
import Expect exposing (Expectation, equalSets)
import Test exposing (..)
import Dict


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
        , test "Success" <|
            \_ ->
                let
                    model =
                        [ ( "module1"
                          , { sourceCode = Just "x = 1"
                            , dirAttempts = Dict.fromList [ ( "dir1", DirSuccess ) ]
                            }
                          )
                        ]
                            |> Dict.fromList
                in
                    moduleStatus model "module1"
                        |> Expect.equal
                            (Success { dirName = "dir1" })
        , test "TotalFail" <|
            \_ ->
                let
                    model =
                        [ ( "module1"
                          , { sourceCode = Nothing
                            , dirAttempts = Dict.fromList [ ( "dir1", DirFail ) ]
                            }
                          )
                        ]
                            -- progress =
                            --     [ ( "module1", [ { dirName = "dir1", status = DirFail } ] )
                            --     ]
                            |> Dict.fromList
                in
                    moduleStatus model "module1"
                        |> Expect.equal
                            (TotalFail)
        , test "atLeastOneSuccess" <|
            \_ ->
                (Dict.fromList
                    [ ( "dir1", DirFail )
                    , ( "dir2", DirSuccess )
                    , ( "dir3", DirSuccess )
                    ]
                )
                    |> atLeastOneSuccess
                    |> Expect.equal (Just "dir2")
        , test "haveNotExhaustedAllOptions" <|
            \_ ->
                (Dict.fromList
                    [ ( "dir1", DirFail )
                    , ( "dir2", DirNotAttemptedYet )
                    , ( "dir3", DirSuccess )
                    ]
                )
                    |> haveNotExhaustedAllOptions
                    |> Expect.equal (Just "dir2")
        ]
