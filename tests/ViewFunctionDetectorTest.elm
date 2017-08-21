module ViewFunctionDetectorTest exposing (..)

import ViewFunctionDetector exposing (isViewFunction)
import Types exposing (Type(Var, Type))
import Test exposing (..)
import Expect


suite : Test
suite =
    describe "qualifiedNameToPath"
        [ test "Foo.Bar.Baz" <|
            \_ ->
                (Type "Html" [ Var "msg" ])
                    |> isViewFunction
                    |> Expect.equal True
        ]



-- type Type
--     = Var String
--     | Lambda Type Type
--     | Tuple (List Type)
--     | Type String (List Type)
--     | Record (List ( String, Type )) (Maybe String)
