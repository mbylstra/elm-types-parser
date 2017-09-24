module FirstPass exposing (..)

import String.Extra
import ElmTypesParser exposing (parseTypeAlias, parseTypeAnnotation, parseUnion)
import ImportStatement exposing (importStatement, parseImportStatement)
import ModuleStatement exposing (parseModuleStatement)
import Parser
import Types
    exposing
        ( Block(Import, IgnoreBlock, TypeAliasDefinition, Union, TypeAnnotation, Module)
        , ImportStatement
        , ModuleStatement
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
        |> String.lines
        |> removeOneLineComments
        |> splitIntoBlocks
        |> classifyBlocks
        |> List.map parseBlock
        |> List.map Result.toMaybe
        |> List.filterMap identity


removeOneLineComments : List String -> List String
removeOneLineComments lines =
    lines
        |> List.map removeOneLineComment


removeOneLineComment : String -> String
removeOneLineComment line =
    line |> String.split "--" |> List.head |> Maybe.withDefault ""


splitIntoBlocks : List String -> List String
splitIntoBlocks lines =
    case lines of
        [] ->
            []

        line :: [] ->
            [ line ]

        line :: restLines ->
            restLines
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
        ImportStatementBlock (replaceNewLinesWithSpaces s)
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
            parseModuleStatement string
                |> Result.map Module

        FunctionDefinitionBlock string ->
            Ok IgnoreBlock

        UnknownBlock string ->
            Ok IgnoreBlock


replaceNewLinesWithSpaces : String -> String
replaceNewLinesWithSpaces s =
    s |> String.Extra.replace "\n" " "
