module FirstPassTest exposing (..)

import Expect exposing (Expectation, equalSets)
import Test exposing (..)
import FirstPass
    exposing
        ( parseModule
        , RawBlock(TypeDefinition, ModuleStatementBlock, EmptyLines)
        , removeOneLineComments
        , splitIntoBlocks
        , classifyBlocks
        )


suite : Test
suite =
    describe "FirstPass.elm"
        [ describe "parseModule"
            [ test "basic test" <|
                \_ ->
                    ([ "module Main exposing (..)"
                     , "type Result error value = Ok value | Err error"
                     ]
                        |> String.join "\n"
                    )
                        |> String.lines
                        |> removeOneLineComments
                        |> splitIntoBlocks
                        |> classifyBlocks
                        |> Expect.equal
                            [ ModuleStatementBlock "module Main exposing (..)"
                            , TypeDefinition "type Result error value = Ok value | Err error"
                            ]
            , test "test with multi line comments" <|
                \_ ->
                    ([ "{-| A `Result` is either `Ok` meaning the computation succeeded, or it is an"
                     , "`Err` meaning that there was some failure."
                     , "-}"
                     , "type Result error value = Ok value | Err error"
                     ]
                        |> String.join "\n"
                    )
                        |> String.lines
                        |> removeOneLineComments
                        |> splitIntoBlocks
                        |> classifyBlocks
                        |> Expect.equal
                            [ EmptyLines "{-| A `Result` is either `Ok` meaning the computation succeeded, or it is an"
                            , EmptyLines "`Err` meaning that there was some failure."
                            , EmptyLines "-}"
                            , TypeDefinition "type Result error value = Ok value | Err error"
                            ]
            ]
        ]
