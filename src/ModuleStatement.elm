module ModuleStatement exposing (..)

import Parser exposing (Count(AtLeast), Parser, zeroOrMore, (|.), (|=))
import ElmTypesParser
    exposing
        ( qualifiedCapVar
        , whitespace
        , lowVar
        , capVar
        , someWhitespace
        )
import Types
    exposing
        ( ModuleStatement
        , Listing
        )


type alias RawDottedName =
    String


parseModuleStatement : String -> Result Parser.Error ModuleStatement
parseModuleStatement string =
    Parser.run importStatement string


importStatement : Parser ModuleStatement
importStatement =
    Parser.succeed identity
        |= importStatementName


importStatementName : Parser String
importStatementName =
    Parser.succeed identity
        |. Parser.symbol "module"
        |. someWhitespace
        |= qualifiedCapVar
