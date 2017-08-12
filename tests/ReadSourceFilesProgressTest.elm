module ReadSourceFilesProgressTest exposing (..)

import ReadSourceFilesProgress exposing (..)
import Expect exposing (Expectation, equalSets)
import Test exposing (..)
import Dict


suite : Test
suite =
    describe "qualifiedNameToPath"
        [ test "HaveNotExhaustedAllOptions" <|
            \_ ->
                let
                    progress =
                        [ ( "module1", [ { dirName = "dir1", status = DirNotOpenedYet } ] )
                        ]
                            |> Dict.fromList
                in
                    moduleStatus progress "module1"
                        |> Expect.equal
                            (HaveNotExhaustedAllOptions { nextDirName = "dir1" })
        , test "Success" <|
            \_ ->
                let
                    progress =
                        [ ( "module1", [ { dirName = "dir1", status = DirSuccess } ] )
                        ]
                            |> Dict.fromList
                in
                    moduleStatus progress "module1"
                        |> Expect.equal
                            (Success { dirName = "dir1" })
        , test "TotalFail" <|
            \_ ->
                let
                    progress =
                        [ ( "module1", [ { dirName = "dir1", status = DirFail } ] )
                        ]
                            |> Dict.fromList
                in
                    moduleStatus progress "module1"
                        |> Expect.equal
                            (TotalFail)
        , test "atLeastOneSuccess" <|
            \_ ->
                [ { dirName = "dir1", status = DirFail }
                , { dirName = "dir2", status = DirSuccess }
                , { dirName = "dir3", status = DirSuccess }
                ]
                    |> atLeastOneSuccess
                    |> Expect.equal (Just "dir2")
        , test "haveNotExhaustedAllOptions" <|
            \_ ->
                [ { dirName = "dir1", status = DirFail }
                , { dirName = "dir2", status = DirNotOpenedYet }
                ]
                    |> haveNotExhaustedAllOptions
                    |> Expect.equal (Just "dir2")
        ]
