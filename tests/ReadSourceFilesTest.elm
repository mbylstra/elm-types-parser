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
                            { moduleName = "Module1"
                            , sourceCode = Nothing
                            , dirAttempts =
                                Dict.fromList
                                    [ ( "dir1", DirNotAttemptedYet )
                                    ]
                            }

                        result =
                            getNextCmds model
                    in
                        result
                            |> Expect.all
                                [ Tuple.first
                                    >> Expect.equal
                                        { moduleName = "Module1"
                                        , sourceCode = Nothing
                                        , dirAttempts =
                                            Dict.fromList
                                                [ ( "dir1", InFlight )
                                                ]
                                        }
                                , Tuple.second >> List.length >> Expect.equal 1
                                ]
            , test "getGoal with Nothing result" <|
                \_ ->
                    let
                        model =
                            { moduleName = "Module1"
                            , sourceCode = Nothing
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
                            , sourceCode = Just "x = 1"
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
                                    , sourceCode = Nothing
                                    , dirAttempts =
                                        Dict.fromList
                                            [ ( "dir1", InFlight )
                                            , ( "dir2", InFlight )
                                            ]
                                    }
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
                                        ]
                                    )
                ]
            )
        ]
