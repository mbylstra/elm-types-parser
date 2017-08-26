module FirstPass exposing (..)

import ElmTypesParser exposing (parseTypeAlias, parseTypeAnnotation, parseUnion)
import ImportStatement exposing (importStatement, parseImportStatement)
import Parser
import Types
    exposing
        ( Block(Import, IgnoreBlock, TypeAliasDefinition, Union, TypeAnnotation)
        , ImportStatement
        )


type RawBlocks
    = List (List String)


type RawBlock
    = EmptyLines String
    | ImportStatementBlock String
    | ModuleStatementBlock String
    | TypeAliasDefinitionBlock String
    | TypeDefinition String
    | TypeAnnotationBlock String
    | FunctionDefinitionBlock String
    | UnknownBlock String


parseModule : String -> List Block
parseModule source =
    source
        |> splitIntoBlocks
        |> classifyBlocks
        |> List.map parseBlock
        |> List.map Result.toMaybe
        |> List.filterMap identity


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


classifyBlock : String -> RawBlock
classifyBlock s =
    if s |> String.startsWith "module" then
        ModuleStatementBlock s
    else if s |> String.startsWith "import" then
        ImportStatementBlock s
    else if s |> String.startsWith "type alias" then
        TypeAliasDefinitionBlock s
    else if s |> String.startsWith "type" then
        TypeDefinition s
    else if s |> String.contains "=" then
        FunctionDefinitionBlock s
    else if s |> String.contains ":" then
        TypeAnnotationBlock s
    else
        EmptyLines s


classifyBlocks : List String -> List RawBlock
classifyBlocks strings =
    strings
        |> List.map classifyBlock


parseBlock : RawBlock -> Result Parser.Error Block
parseBlock rawBlock =
    case rawBlock of
        EmptyLines string ->
            Ok <| IgnoreBlock

        ImportStatementBlock string ->
            parseImportStatement string
                |> Result.map Import

        TypeAliasDefinitionBlock string ->
            parseTypeAlias string
                |> Result.map TypeAliasDefinition

        TypeDefinition string ->
            parseUnion string
                |> Result.map Union

        TypeAnnotationBlock string ->
            parseTypeAnnotation string
                |> Result.map TypeAnnotation

        ModuleStatementBlock string ->
            Ok IgnoreBlock

        FunctionDefinitionBlock string ->
            Ok IgnoreBlock

        UnknownBlock string ->
            Ok IgnoreBlock
