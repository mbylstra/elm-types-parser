module ViewFunctionDetector exposing (..)

import Types exposing (Type(..))


{-| This is a *really* dumb version to begine with that looks for the existence of Html
-}
isViewFunction : Type -> Bool
isViewFunction tipe =
    case tipe of
        Var _ ->
            False

        Lambda _ right ->
            isViewFunction right

        Tuple _ ->
            False

        Type typeName _ ->
            typeName == "Html"

        Record _ _ ->
            False
