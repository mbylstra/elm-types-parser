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
        , getGoal
        , readElmModule
        , init
        )
import Test exposing (..)


suite : Test
suite =
    describe "ReadSourceFiles"
        [ describe "qualifiedNameToPath"
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
            , test "getGoal with Nothing result" <|
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
                        getGoal model
                            |> Expect.equal
                                (Err model)
            , test "getGoal with a result" <|
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
                        getGoal model
                            |> Expect.equal
                                (Ok <| Dict.fromList [ ( "module1", "x = 1" ) ])
            ]
        , describe "init"
            (let
                initArg =
                    { moduleNames = [ "Foo", "Bar" ], sourceDirectories = [ "dir1", "dir2" ] }
             in
                [ test "model" <|
                    \_ ->
                        let
                            ( model, _ ) =
                                init initArg
                        in
                            model
                                |> Expect.equal
                                    (Dict.fromList
                                        [ ( "Bar"
                                          , { sourceCode = Nothing
                                            , dirAttempts =
                                                Dict.fromList
                                                    [ ( "dir1", InFlight )
                                                    , ( "dir2", InFlight )
                                                    ]
                                            }
                                          )
                                        , ( "Foo"
                                          , { sourceCode = Nothing
                                            , dirAttempts =
                                                Dict.fromList
                                                    [ ( "dir1", InFlight )
                                                    , ( "dir2", InFlight )
                                                    ]
                                            }
                                          )
                                        ]
                                    )
                , test "cmd" <|
                    \_ ->
                        let
                            ( _, cmd ) =
                                init initArg
                        in
                            cmd
                                |> Expect.equal
                                    (Cmd.batch
                                        [ readElmModule
                                            { path = "dir2/Foo.elm"
                                            , portScope =
                                                { path = "dir2/Foo.elm"
                                                , dir = "dir2"
                                                , moduleName = "Foo"
                                                }
                                            }
                                        , readElmModule
                                            { path = "dir1/Foo.elm"
                                            , portScope =
                                                { path = "dir1/Foo.elm"
                                                , dir = "dir1"
                                                , moduleName = "Foo"
                                                }
                                            }
                                        , readElmModule
                                            { path = "dir2/Bar.elm"
                                            , portScope =
                                                { path = "dir2/Bar.elm"
                                                , dir = "dir2"
                                                , moduleName = "Bar"
                                                }
                                            }
                                        , readElmModule
                                            { path = "dir1/Bar.elm"
                                            , portScope =
                                                { path = "dir1/Bar.elm"
                                                , dir = "dir1"
                                                , moduleName = "Bar"
                                                }
                                            }
                                        ]
                                    )
                ]
            )
        ]



-- dummyCmd : Cmd msg
-- dummyCmd =
--
--
--         { type = "node", branches = [{ type = "leaf", home = "readElmModule", value =  }] }
