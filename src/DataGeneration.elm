module DataGeneration exposing (..)

import Types exposing (..)


generateData : Type -> String
generateData tipe =
    case tipe of
        Var varName ->
            "()"

        Lambda leftTipe rightTipe ->
            let
                left =
                    generateData leftTipe
            in
                case rightTipe of
                    Lambda _ _ ->
                        left ++ " " ++ (generateData rightTipe)

                    _ ->
                        left

        Tuple tipes ->
            "("
                ++ (tipes |> List.map generateData |> String.join ", ")
                ++ ")"

        Type typeName _ ->
            case typeName of
                "Int" ->
                    "1"

                "String" ->
                    "\"a\""

                "Bool" ->
                    "True"

                "Float" ->
                    "1.0"

                _ ->
                    Debug.crash "unknown type"

        Record fields _ ->
            let
                generateFieldData ( name, tipe ) =
                    name ++ " = " ++ (generateData tipe)
            in
                "{"
                    ++ (fields
                            |> List.map generateFieldData
                            |> String.join ", "
                       )
                    ++ "}"
