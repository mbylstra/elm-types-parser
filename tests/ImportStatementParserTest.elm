module ImportStatementParserTest exposing (..)

-- import Expect exposing (Expectation)
-- import Test exposing (..)
-- import Parser
-- examples
-- import Date exposing (Date)
-- import RemoteData exposing (RemoteData(..), WebData)
-- import I18n
-- import RemoteData.Http as Http
-- import Html exposing (..)
-- import Html.Attributes exposing (href, src)
-- import Html.Events exposing (..)
-- import Styles exposing (..)
-- import Types exposing (TacoUpdate(..), Taco, Commit, Stargazer)
-- import Decoders
-- | A listing of values. Something like (a,b,c) or (..) or (a,b,..)
-- LISTINGS


type alias Listing a =
    { explicits : List a
    , open : Bool
    }


openListing : Listing a
openListing =
    Listing [] True


closedListing : Listing a
closedListing =
    Listing [] False


listing : List a -> Listing a
listing xs =
    Listing xs False



-- type alias ImportMethod =
--     { alias : Maybe Text
--     , exposedVars : !(Var.Listing Var.Value)
--     }
--
-- type alias ImportStatement =
--     { modulePath : List String
--     , aliasedAs : Maybe String
--     , exposing: Maybe Exposing
--     }
--
-- type Exposing =
--     WildCard | List
--
--
-- suite : Test
-- suite =
--     describe "DocTypeParser"
--         [ test "works" <|
--             \_ ->
--                 "Int"
--                     |> DocTypeParser.parse
--                     |> Expect.equal
--                         (Ok <|
--                             Type "Int" []
--                         )
--         ]
