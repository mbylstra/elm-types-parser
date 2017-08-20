module ReadSourceFilesTest exposing (..)

import Dict
import Expect exposing (Expectation, equalSets)
import ReadSourceFiles
    exposing
        ( DirAttempt(DirFail, DirNotAttemptedYet, DirSuccess, InFlight)
        , Model
        , atLeastOneSuccess
        , haveNotExhaustedAllOptions
        , getNextCmds
        , getResult
        )
import Test exposing (..)


suite : Test
suite =
    describe "qualifiedNameToPath"
        [ test "atLeastOneSuccess" <|
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
        , test "getNextCmds" <|
            \_ ->
                let
                    model : Model
                    model =
                        [ ( "module1"
                          , { sourceCode = Nothing
                            , dirAttempts =
                                Dict.fromList
                                    [ ( "dir1", DirNotAttemptedYet )
                                    ]
                            }
                          )
                        ]
                            |> Dict.fromList

                    result =
                        getNextCmds model
                in
                    result
                        |> Expect.all
                            [ Tuple.first
                                >> Expect.equal
                                    ([ ( "module1"
                                       , { sourceCode = Nothing
                                         , dirAttempts =
                                            Dict.fromList
                                                [ ( "dir1", InFlight )
                                                ]
                                         }
                                       )
                                     ]
                                        |> Dict.fromList
                                    )
                            , Tuple.second >> List.length >> Expect.equal 1
                            ]
        , test "getResult with Nothing result" <|
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
                    getResult model
                        |> Expect.equal
                            (Err model)
        , test "getResult with a result" <|
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
                    getResult model
                        |> Expect.equal
                            (Ok <| Dict.fromList [ ( "module1", "x = 1" ) ])
        ]
