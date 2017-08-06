module Helpers exposing (..)

-- import PackageInfo

import String
import List.Extra exposing (uncons)


qualifiedNameToPath : String -> String
qualifiedNameToPath name =
    name
        |> String.split "."
        |> List.reverse
        |> uncons
        |> Maybe.map
            (\( fileNameNoExt, reverseBasePath ) ->
                let
                    fileName =
                        fileNameNoExt ++ ".elm"

                    fullReversePath =
                        fileName :: reverseBasePath

                    path =
                        List.reverse fullReversePath
                in
                    String.join "/" path
            )
        -- String.split always returns at least one element, so this can't happen
        |> Maybe.withDefault ""
