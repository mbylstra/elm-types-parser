module Mane exposing (..)

import ElmTypesParser
    exposing
        ( parseTypeAlias
        , parseUnion
        , parseTypeConstructor
        , parseTypeConstructors
        , someWhitespace
        )
import Types exposing (Type(..))


-- import ElmTypesParser exposing (tipe)

import Expect exposing (Expectation, equalSets)
import Parser exposing (Parser, (|.), (|=))


-- import Parser.LanguageKit as LanguageKit
-- import Char

import Result.Extra exposing (isErr)
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

        UnionDefinition _ ->
            Debug.crash "Union type generation is not supported yet"


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
                    |> ElmTypesParser.parseTipe
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
                    |> ElmTypesParser.parseTipe
                    |> Result.map generateData
                    |> Expect.equal (Ok "1 True")
        , test "someWhitespace 1" <|
            \_ ->
                ""
                    |> Parser.run someWhitespace
                    |> isErr
                    |> Expect.equal True
        , test "someWhitespace 2" <|
            \_ ->
                "\n"
                    |> Parser.run someWhitespace
                    |> isErr
                    |> Expect.equal False
        , test "someWhitespace 3" <|
            \_ ->
                "\n\n"
                    |> Parser.run someWhitespace
                    |> isErr
                    |> Expect.equal False

        -- , test "someWhitespace 3" <|
        --     \_ ->
        --         "\n --comment"
        --             |> Parser.run someWhitespace
        --             |> isErr
        --             |> Expect.equal False
        -- , test "someWhitespace 4" <|
        --     -- This is expected, but it should be improved so that a comment counts as whitespace
        --     \_ ->
        --         "--comment"
        --             |> Parser.run someWhitespace
        --             |> isErr
        --             |> Expect.equal True
        -- , test "record" <|
        --     \_ ->
        --         let
        --             s =
        --                 """
        --           { email : String
        --           , password : String
        --           , loading : Bool
        --           , error : Bool
        --           }
        --           """
        --         in
        --             s
        --                 |> ElmTypesParser.parse
        --                 |> Result.map generateData
        --                 |> Expect.equal (Ok "1 True")
        , test "splitIntoBlocks" <|
            \_ ->
                "aaa\n aaa\nbbb\nccc"
                    |> splitIntoBlocks
                    |> Expect.equal
                        [ "aaa\n aaa"
                        , "bbb"
                        , "ccc"
                        ]

        -- , test "classifyBlocks" <|
        --     \_ ->
        --         "module Blah exposing (..)\nimport String\n\ntype alias Id = Int\ntype MyType = MyType\nx : Int\nx = 5\n\n"
        --             |> splitIntoBlocks
        --             |> List.map classifyBlock
        --             |> Expect.equal
        --                 [ ModuleStatement, ImportStatement, TypeAnnotation, FunctionDefinition ]
        -- , test "parse type alias" <|
        --     \_ ->
        --         "type alias Id = Int"
        --             |> parseTypeAlias
        --             |> Expect.equal (Ok ( "Id", Type "Int" [] ))
        , test "typeConstructor: takes no args" <|
            \_ ->
                "TypeA"
                    |> parseTypeConstructor
                    |> Expect.equal
                        (Ok
                            ( "TypeA", [] )
                        )
        , test "typeConstructor: takes one simple Type arg" <|
            \_ ->
                "TypeA Int"
                    |> parseTypeConstructor
                    |> Expect.equal
                        (Ok
                            ( "TypeA", [ Type "Int" [] ] )
                        )
        , test "typeConstructor: takes two simple Type args" <|
            \_ ->
                "MyType ArgA ArgB"
                    |> parseTypeConstructor
                    |> Expect.equal
                        (Ok
                            ( "MyType", [ Type "ArgA" [], Type "ArgB" [] ] )
                        )
        , test "typeConstructor: takes three simple Type args" <|
            \_ ->
                "MyType ArgA ArgB ArgC"
                    |> parseTypeConstructor
                    |> Expect.equal
                        (Ok
                            ( "MyType", [ Type "ArgA" [], Type "ArgB" [], Type "ArgC" [] ] )
                        )
        , test "typeConstructor: takes two type variables as args" <|
            \_ ->
                "MyType a b"
                    |> parseTypeConstructor
                    |> Expect.equal
                        (Ok
                            ( "MyType", [ Var "a", Var "b" ] )
                        )
        , test "typeConstructor: takes a tuple arg" <|
            \_ ->
                "MyType (Int, String)"
                    |> parseTypeConstructor
                    |> Expect.equal
                        (Ok
                            ( "MyType", [ Tuple [ Type "Int" [], Type "String" [] ] ] )
                        )
        , test "typeConstructors: with one that doesn't take any args" <|
            \_ ->
                "MyType"
                    |> parseTypeConstructors
                    |> Expect.equal
                        (Ok
                            [ ( "MyType", [] ) ]
                        )
        , test "typeConstructors: with one arg, and it takes a Unit type" <|
            \_ ->
                "TypeA ()"
                    |> parseTypeConstructors
                    |> Expect.equal
                        (Ok
                            [ ( "TypeA", [ Tuple [] ] ) ]
                        )
        , test "typeConstructors: Two of them. Both take no args." <|
            \_ ->
                "TypeA | TypeB"
                    |> parseTypeConstructors
                    |> Expect.equal
                        (Ok
                            [ ( "TypeA", [] )
                            , ( "TypeB", [] )
                            ]
                        )
        , test "typeConstructors: Two of them.  First takes a type variable as an arg." <|
            \_ ->
                "TypeA a | TypeB"
                    |> parseTypeConstructors
                    |> Expect.equal
                        (Ok
                            [ ( "TypeA", [ Var "a" ] )
                            , ( "TypeB", [] )
                            ]
                        )
        , test "typeConstructors: Three of them.  First two take a type variable as an arg." <|
            \_ ->
                "TypeA a | TypeB b | TypeC"
                    |> parseTypeConstructors
                    |> Expect.equal
                        (Ok
                            [ ( "TypeA", [ Var "a" ] )
                            , ( "TypeB", [ Var "b" ] )
                            , ( "TypeC", [] )
                            ]
                        )
        , test "typeConstructors: Two of them.  First takes a simple Type as an arg." <|
            \_ ->
                "TypeA Int | TypeB"
                    |> parseTypeConstructors
                    |> Expect.equal
                        (Ok
                            [ ( "TypeA", [ Type "Int" [] ] )
                            , ( "TypeB", [] )
                            ]
                        )
        , test "typeConstructors: Two of them.  Both take a simple Type as an arg." <|
            \_ ->
                "TypeA Int | TypeB String"
                    |> parseTypeConstructors
                    |> Expect.equal
                        (Ok
                            [ ( "TypeA", [ Type "Int" [] ] )
                            , ( "TypeB", [ Type "String" [] ] )
                            ]
                        )
        , test "unionType: single constructor that takes one arg" <|
            \_ ->
                "type MyType = TypeA Int"
                    |> parseUnion
                    |> Expect.equal
                        (Ok
                            ( "MyType"
                            , [ ( "TypeA", [ Type "Int" [] ] )
                              ]
                            )
                        )
        , test "unionType: single constructor that takes two args" <|
            \_ ->
                "type MyType = TypeA Int String"
                    |> parseUnion
                    |> Expect.equal
                        (Ok
                            ( "MyType"
                            , [ ( "TypeA", [ Type "Int" [], Type "String" [] ] )
                              ]
                            )
                        )
        , test "unionType: single constructor that takes no args" <|
            \_ ->
                "type MyType = TypeA"
                    |> parseUnion
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
                    |> parseUnion
                    |> Expect.equal
                        (Ok
                            ( "MyType"
                            , [ ( "TypeA", [] )
                              , ( "TypeB", [] )
                              ]
                            )
                        )
        ]
