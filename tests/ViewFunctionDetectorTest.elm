module ViewFunctionDetectorTest exposing (..)

import ViewFunctionDetector exposing (isViewFunction)
import Types exposing (Type(Var, Type, Lambda))
import Test exposing (..)
import Expect


suite : Test
suite =
    describe "qualifiedNameToPath"
        [ test "static view function" <|
            \_ ->
                (Type "Html" [ Var "msg" ])
                    |> isViewFunction
                    |> Expect.equal True
        , test "view function that takes an int" <|
            \_ ->
                (Lambda (Type "Int" []) (Type "Html" [ Var "msg" ]))
                    |> isViewFunction
                    |> Expect.equal True
        , test "view function that takes an int (but not really a Html function)" <|
            \_ ->
                (Lambda (Type "Int" []) (Type "NotHtml" [ Var "msg" ]))
                    |> isViewFunction
                    |> Expect.equal False
        ]



-- type Type
--     = Var String
--     | Lambda Type Type
--     | Tuple (List Type)
--     | Type String (List Type)
--     | Record (List ( String, Type )) (Maybe String)
