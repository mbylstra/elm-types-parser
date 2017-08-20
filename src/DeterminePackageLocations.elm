port module DeterminePackageLocations exposing (..)

import Dict exposing (Dict)
import PackageInfo exposing (PackageInfo)
import PackageInfo.Version as Version exposing (Version)
import PackageInfo.VersionRange exposing (VersionRange)
import Path.Posix as Path exposing (joinPath)
import Maybe.Extra exposing (isJust)


-- type alias Model =
--     Dict ModuleName (List { dirName : String, status : DirStatus })
-- type alias ModuleName =
--     String


type Msg
    = GetFilenamesInDirResult GetFilenamesInDirResultR


type alias GetFilenamesInDirResultR =
    { filenames : List String
    , scope : GetFilenamesInDirResultScope
    }


type alias GetFilenamesInDirResultScope =
    { package_ : ( String, String )
    , versionRange : VersionRange
    }


port getFilenamesInDir :
    { path : String
    , scope : GetFilenamesInDirResultScope
    }
    -> Cmd msg


port getFilenamesInDirResult : (GetFilenamesInDirResultR -> msg) -> Sub msg


type alias Model =
    Dict ( String, String ) (Maybe String)


init : PackageInfo -> ( Model, Cmd Msg )
init packageInfo =
    let
        packages : List { package : ( String, String ), versionRange : VersionRange }
        packages =
            packageInfo.dependencies
                |> List.map
                    (\{ name, versionRange } ->
                        { package = splitPackageName name, versionRange = versionRange }
                    )

        model =
            packages
                |> List.map (.package >> \package -> ( package, Nothing ))
                |> Dict.fromList

        cmds =
            packages
                |> List.map
                    (\package ->
                        getFilenamesInDir
                            { path = getPackageUsernameDir package.package
                            , scope = { package_ = package.package, versionRange = package.versionRange }
                            }
                    )

        -- _ =
        --     Debug.log "cmds" cmds
    in
        model ! cmds


getPackageUsernameDir : ( String, String ) -> String
getPackageUsernameDir ( username, repo ) =
    joinPath [ "./elm-stuff/packages/", username, repo ]


getPackageDir : ( String, String ) -> String -> String
getPackageDir ( username, repo ) version =
    joinPath [ getPackageUsernameDir ( username, repo ), version, "src" ]


splitPackageName : String -> ( String, String )
splitPackageName packageName =
    case String.split "/" packageName of
        username :: repo :: [] ->
            ( username, repo )

        _ ->
            ( "", "" )


subscriptions : Sub Msg
subscriptions =
    getFilenamesInDirResult GetFilenamesInDirResult


update : Msg -> Model -> ( Model, Maybe (List String) )
update msg model =
    let
        newModel =
            case msg of
                GetFilenamesInDirResult { filenames, scope } ->
                    case (Debug.log "version" getMostRecentValidVersion scope.versionRange filenames) of
                        Just version ->
                            let
                                dir =
                                    getPackageDir scope.package_ (Version.toString version)
                            in
                                model
                                    |> Dict.insert scope.package_ (Just dir)

                        Nothing ->
                            model

        outValue =
            if finished newModel then
                Dict.values newModel
                    |> List.filterMap identity
                    |> Just
            else
                Nothing
    in
        ( newModel, outValue )


finished : Model -> Bool
finished model =
    (Debug.log "model" model) |> Dict.values |> List.all isJust


getMostRecentValidVersion : VersionRange -> List String -> Maybe Version
getMostRecentValidVersion versionRange versionStrings =
    let
        versions : List Version
        versions =
            versionStrings
                |> List.map
                    (Version.fromString >> Result.withDefault (Version 0 0 0))
    in
        versions
            |> List.reverse
            |> List.foldl
                (\version prev ->
                    case inRange versionRange version of
                        True ->
                            Just version

                        False ->
                            prev
                )
                Nothing



--         -- TODO: these should be sorted first!


inRange : VersionRange -> Version -> Bool
inRange { minimum, maximum } version =
    versionGTE version minimum && versionLT version maximum


versionLT : Version -> Version -> Bool
versionLT a b =
    case Version.compare a b of
        LT ->
            True

        _ ->
            False


versionGTE : Version -> Version -> Bool
versionGTE a b =
    versionLT a b |> not



-- let
--     (username, repo) = scope
--
-- TODO
-- get the min and max version numbers
-- split the package name into { username : String, repo: String, etc }
-- get all version numbers in the directory
-- sort them somehow
-- get the largest version number within min and less than max
-- that's it!
-- it will be tricky because there's a lot of IO to be done :(
-- Get the list of dirnames in one go I think
---- Maybe just do it all in node! Yikes!
--  This could be a separate module
