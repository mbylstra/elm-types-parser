module Mane exposing (..)

import ElmTypesParser exposing (Type(..), parseTypeAlias, separatedBy, parseUnionType)


-- import ElmTypesParser exposing (tipe)

import Expect exposing (Expectation, equalSets)
import Parser exposing (Parser, (|.), (|=))


-- import Parser.LanguageKit as LanguageKit
-- import Char
-- import Result.Extra exposing (isErr)

import Test exposing (..)


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


splitIntoBlocks : String -> List String
splitIntoBlocks elmCode =
    case elmCode |> String.lines of
        [] ->
            []

        line :: [] ->
            [ line ]

        line :: lines ->
            lines
                |> List.foldl
                    (\line { blocks, currBlock } ->
                        if (line |> String.startsWith " ") then
                            { blocks = blocks
                            , currBlock = currBlock ++ "\n" ++ line
                            }
                        else
                            { blocks = blocks ++ [ currBlock ]
                            , currBlock = line
                            }
                    )
                    { blocks = [], currBlock = line }
                |> \{ blocks, currBlock } ->
                    blocks ++ [ currBlock ]


type Block
    = EmptyLines
    | ImportStatement
    | ModuleStatement
    | TypeAliasDefinition
    | TypeDefinition
    | TypeAnnotation
    | FunctionDefinition
    | Unknown


type RawBlocks
    = List (List String)


classifyBlock : String -> Block
classifyBlock s =
    if s |> String.startsWith "module" then
        ModuleStatement
    else if s |> String.startsWith "import" then
        ImportStatement
    else if s |> String.startsWith "type alias" then
        TypeAliasDefinition
    else if s |> String.startsWith "type" then
        TypeDefinition
    else if s |> String.contains "=" then
        FunctionDefinition
    else if s |> String.contains ":" then
        TypeAnnotation
    else
        EmptyLines


classifyBlocks : List String -> List ( Block, String )
classifyBlocks strings =
    strings
        |> List.map (\string -> ( classifyBlock string, string ))


suite : Test
suite =
    describe "ElmTypesParser"
        [ test "works" <|
            \_ ->
                "Int"
                    |> ElmTypesParser.parse
                    |> Expect.equal
                        (Ok <|
                            Type "Int" []
                        )

        -- , test "complex one" <|
        --     \_ ->
        --         "(Int -> a) -> { x : Int, y : { z : String }}"
        --             |> ElmTypesParser.parse
        --             |> toString
        --             |> Expect.equal "asdasd"
        , test "generateData" <|
            \_ ->
                "Int -> Bool -> Html Msg"
                    |> ElmTypesParser.parse
                    |> Result.map generateData
                    |> Expect.equal (Ok "1 True")
        , test "record" <|
            \_ ->
                let
                    s =
                        """
                  { email : String
                  , password : String
                  , loading : Bool
                  , error : Bool
                  }
                  """
                in
                    s
                        |> ElmTypesParser.parse
                        |> Result.map generateData
                        |> Expect.equal (Ok "1 True")
        , test "splitIntoBlocks" <|
            \_ ->
                "aaa\n aaa\nbbb\nccc"
                    |> splitIntoBlocks
                    |> Expect.equal
                        [ "aaa\n aaa"
                        , "bbb"
                        , "ccc"
                        ]
        , test "classifyBlocks" <|
            \_ ->
                "module Blah exposing (..)\nimport String\n\ntype alias Id = Int\ntype MyType = MyType\nx : Int\nx = 5\n\n"
                    |> splitIntoBlocks
                    |> List.map classifyBlock
                    |> Expect.equal
                        [ ModuleStatement, ImportStatement, TypeAnnotation, FunctionDefinition ]
        , test "parse type alias" <|
            \_ ->
                "type alias Id = Int"
                    |> parseTypeAlias
                    |> Expect.equal (Ok ( "Id", Type "Int" [] ))
        , test "separatedBy" <|
            \_ ->
                let
                    parser =
                        separatedBy (Parser.symbol "|") Parser.int
                in
                    "1|2|3"
                        |> Parser.run parser
                        |> Expect.equal (Ok [ 1, 2, 3 ])
        , test "separatedBy2" <|
            \_ ->
                let
                    parser =
                        separatedBy (Parser.symbol "|") Parser.int
                in
                    "1"
                        |> Parser.run parser
                        |> Expect.equal (Ok [ 1 ])

        -- We need union type constructor tests here
        , test "unionType with single constructor that takes one arg" <|
            \_ ->
                "type MyType = TypeA Int"
                    |> parseUnionType
                    |> Expect.equal
                        (Ok
                            ( "MyType"
                            , [ ( "TypeA", [ Type "Int" [] ] )
                              ]
                            )
                        )
        , test "unionType with single constructor that takes two args" <|
            \_ ->
                "type MyType = TypeA Int String"
                    |> parseUnionType
                    |> Expect.equal
                        (Ok
                            ( "MyType"
                            , [ ( "TypeA", [ Type "Int" [], Type "String" [] ] )
                              ]
                            )
                        )
        , test "unionType with single constructor that takes no args" <|
            \_ ->
                "type MyType = TypeA"
                    |> parseUnionType
                    |> Expect.equal
                        (Ok
                            ( "MyType"
                            , [ ( "TypeA", [] )
                              ]
                            )
                        )
        , test "unionType with two constructors that take no args" <|
            \_ ->
                "type MyType = TypeA | TypeB"
                    |> parseUnionType
                    |> Expect.equal
                        (Ok
                            ( "MyType"
                            , [ ( "TypeA", [] )
                              , ( "TypeB", [] )
                              ]
                            )
                        )
        ]
