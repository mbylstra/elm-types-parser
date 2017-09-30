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
        , getNextCmdsForDirAttempts
        )
import Test exposing (..)


suite : Test
suite =
    describe "ReadSourceFiles"
        [ describe "misc"
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
                            { moduleName = "Module1"
                            , maybeSourceCode = Nothing
                            , dirAttempts =
                                Dict.fromList
                                    [ ( "dir1", DirNotAttemptedYet )
                                    ]
                            }

                        result =
                            getNextCmds { model = model, maxCmdsReached = False }
                    in
                        result
                            |> Expect.all
                                [ Tuple.first
                                    >> Expect.equal
                                        { moduleName = "Module1"
                                        , maybeSourceCode = Nothing
                                        , dirAttempts =
                                            Dict.fromList
                                                [ ( "dir1", InFlight )
                                                ]
                                        }
                                , Tuple.second >> (Expect.notEqual [])
                                ]
            , test "getGoal with Nothing result" <|
                \_ ->
                    let
                        model =
                            { moduleName = "Module1"
                            , maybeSourceCode = Nothing
                            , dirAttempts = Dict.fromList [ ( "dir1", DirNotAttemptedYet ) ]
                            }
                    in
                        getGoal model
                            |> Expect.equal
                                (Err model)
            , test "getGoal with a result" <|
                \_ ->
                    let
                        model =
                            { moduleName = "Module1"
                            , maybeSourceCode = Just "x = 1"
                            , dirAttempts = Dict.fromList [ ( "dir1", DirSuccess ) ]
                            }
                    in
                        getGoal model
                            |> Expect.equal
                                (Ok "x = 1")
            ]
        , describe "init"
            (let
                initArg =
                    { moduleName = "Foo", sourceDirectories = [ "dir1", "dir2" ] }
             in
                [ test "model" <|
                    \_ ->
                        let
                            ( model, _ ) =
                                init initArg
                        in
                            model
                                |> Expect.equal
                                    { moduleName = "Foo"
                                    , maybeSourceCode = Nothing
                                    , dirAttempts =
                                        Dict.fromList
                                            [ ( "dir1", InFlight )
                                            , ( "dir2", DirNotAttemptedYet )
                                            ]
                                    }
                , test "cmd" <|
                    \_ ->
                        let
                            ( _, maybeCmd ) =
                                init initArg
                        in
                            maybeCmd
                                |> Expect.equal
                                    [ readElmModule
                                        { path = "dir1/Foo.elm"
                                        , portScope =
                                            { path = "dir1/Foo.elm"
                                            , dir = "dir1"
                                            , moduleName = "Foo"
                                            }
                                        }
                                    ]
                ]
            )
        , describe "getNextCmdForDirAttempts"
            (let
                dirAttempts =
                    Dict.fromList
                        [ ( "dir1", DirNotAttemptedYet )
                        , ( "dir2", DirNotAttemptedYet )
                        ]
             in
                [ test "none attempted yet" <|
                    \_ ->
                        getNextCmdsForDirAttempts "Foo" dirAttempts
                            |> Expect.equal
                                ( Dict.fromList
                                    [ ( "dir1", InFlight )
                                    , ( "dir2", DirNotAttemptedYet )
                                    ]
                                , [ readElmModule
                                        { path = "dir1/Foo.elm"
                                        , portScope =
                                            { path = "dir1/Foo.elm"
                                            , dir = "dir1"
                                            , moduleName = "Foo"
                                            }
                                        }
                                  ]
                                )
                , test "one in flight" <|
                    \_ ->
                        let
                            dirAttempts =
                                Dict.fromList
                                    [ ( "dir1", InFlight )
                                    , ( "dir2", DirNotAttemptedYet )
                                    ]
                        in
                            getNextCmdsForDirAttempts "Foo" dirAttempts
                                |> Expect.equal
                                    ( Dict.fromList
                                        [ ( "dir1", InFlight )
                                        , ( "dir2", InFlight )
                                        ]
                                    , [ readElmModule
                                            { path = "dir2/Foo.elm"
                                            , portScope =
                                                { path = "dir2/Foo.elm"
                                                , dir = "dir2"
                                                , moduleName = "Foo"
                                                }
                                            }
                                      ]
                                    )
                ]
            )
        ]
