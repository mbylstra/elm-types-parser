module FirstPass exposing (..)


type RawBlocks
    = List (List String)


type Block
    = EmptyLines
    | ImportStatement
    | ModuleStatement
    | TypeAliasDefinition
    | TypeDefinition
    | TypeAnnotation
    | FunctionDefinition
    | Unknown


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
