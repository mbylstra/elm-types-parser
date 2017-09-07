module ModuleInfoTest exposing (..)

import Expect exposing (Expectation, equalSets)
import Test exposing (..)
import Types exposing (Type(Type, Lambda))
import Types exposing (Type(Type, Lambda))
import ModuleInfo exposing (getNames)


suite : Test
suite =
    describe "ModuleInfo"
        [ test "getNames (for Maybe Int)" <|
            \_ ->
                (Type "Maybe" [ Type "Int" [] ])
                    |> getNames
                    |> Expect.equal
                        [ "Maybe", "Int" ]
        ]



-- TODO: test this
-- frame : Bool -> Maybe User -> ActivePage -> Html msg -> Html msg
