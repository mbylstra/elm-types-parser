module DataGenerationTest2 exposing (..)

import DataGeneration exposing (generateFromUnionType, generateViewFunction, generateViewFunctions)
import Dict
import Expect exposing (Expectation, equalSets)
import Helpers exposing (unsafeDictGet)
import Test exposing (..)
import ToQualified exposing (qualifyAllTypes)
import Types exposing (..)


moduleInfos =
    Dict.fromList
        [ ( "Components.LaunchDialog.LaunchDialog"
          , { localUnionTypes = Dict.fromList [ ( "DateField", { name = "DateField", typeVars = [], definition = [ ( "ResponsesDue", [] ), ( "InterviewDue", [] ) ] } ), ( "Msg", { name = "Msg", typeVars = [], definition = [ ( "OpenPicker", [ Type "DateField" [] ] ), ( "DatePickerMsg", [ Type "DateField" [], Type "DatePicker.Msg" [] ] ), ( "ClosePickers", [] ), ( "NoOp", [] ) ] } ) ]
            , localTypeAliases = Dict.fromList [ ( "Configuration", Record ([ ( "mainMsg", Lambda (Type "String" []) (Lambda (Type "Date" []) (Lambda (Type "Date" []) (Var "msg"))) ), ( "mappingMsg", Lambda (Type "Msg" []) (Var "msg") ), ( "closeMsg", Var "msg" ), ( "headerContent", Type "List" ([ Type "Html" ([ Var "msg" ]) ]) ) ]) Nothing ), ( "State", Record ([ ( "responses", Type "Date" [] ), ( "interview", Type "Date" [] ), ( "currentDate", Type "Maybe" ([ Type "Date" [] ]) ), ( "responsesPicker", Type "Maybe" ([ Type "DatePicker.DatePicker" [] ]) ), ( "interviewPicker", Type "Maybe" ([ Type "DatePicker.DatePicker" [] ]) ), ( "path", Type "String" [] ), ( "subject", Type "String" [] ), ( "dialogState", Type "Dialog.State" [] ) ]) Nothing ) ]
            , viewFunctions =
                Dict.fromList
                    [ ( "dateView", Lambda (Type "Maybe" ([ Type "DatePicker.DatePicker" [] ])) (Lambda (Type "Maybe" ([ Type "Date" [] ])) (Lambda (Type "Date" []) (Lambda (Type "DateField" []) (Type "Html" ([ Type "Msg" [] ]))))) )
                    , ( "displayView", Lambda (Type "Maybe" ([ Type "Date" [] ])) (Lambda (Type "Date" []) (Lambda (Type "DateField" []) (Type "Html" ([ Type "Msg" [] ])))) )
                    , ( "pickerView", Lambda (Type "DatePicker.DatePicker" []) (Lambda (Type "DateField" []) (Type "Html" ([ Type "Msg" [] ]))) )
                    , ( "view", Lambda (Type "Configuration" ([ Var "msg" ])) (Lambda (Type "State" []) (Type "Html" ([ Var "msg" ]))) )
                    ]
            , externalNamesModuleInfo = Dict.fromList [ ( "Date", { dottedModulePath = "Date", name = "Date" } ), ( "DatePicker.DatePicker", { dottedModulePath = "DatePicker", name = "DatePicker" } ), ( "DatePicker.Msg", { dottedModulePath = "DatePicker", name = "Msg" } ), ( "Dialog.State", { dottedModulePath = "Dialog", name = "State" } ), ( "Maybe", { dottedModulePath = "Maybe", name = "Maybe" } ) ]
            , dottedModulePath = "Components.LaunchDialog.LaunchDialog"
            }
          )
        , ( "Date", { localUnionTypes = Dict.fromList [ ( "Date", { name = "Date", typeVars = [], definition = [ ( "Date", [] ) ] } ), ( "Day", { name = "Day", typeVars = [], definition = [ ( "Mon", [] ), ( "Tue", [] ), ( "Wed", [] ), ( "Thu", [] ), ( "Fri", [] ), ( "Sat", [] ), ( "Sun", [] ) ] } ), ( "Month", { name = "Month", typeVars = [], definition = [ ( "Jan", [] ), ( "Feb", [] ), ( "Mar", [] ), ( "Apr", [] ), ( "May", [] ), ( "Jun", [] ), ( "Jul", [] ), ( "Aug", [] ), ( "Sep", [] ), ( "Oct", [] ), ( "Nov", [] ), ( "Dec", [] ) ] } ) ], localTypeAliases = Dict.fromList [], externalNamesModuleInfo = Dict.fromList [], viewFunctions = Dict.fromList [], dottedModulePath = "Date" } )
        , ( "DatePicker", { localUnionTypes = Dict.fromList [ ( "DatePicker", { name = "DatePicker", typeVars = [], definition = [ ( "DatePicker", [ Type "Model" [] ] ) ] } ), ( "Msg", { name = "Msg", typeVars = [], definition = [ ( "CurrentDate", [ Type "Date" [] ] ), ( "NextMonth", [] ), ( "PrevMonth", [] ), ( "Pick", [ Type "Date" [] ] ), ( "Change", [ Type "String" [] ] ), ( "Focus", [] ), ( "Blur", [] ), ( "MouseDown", [] ), ( "MouseUp", [] ) ] } ) ], localTypeAliases = Dict.fromList [ ( "Model", Record ([ ( "open", Type "Bool" [] ), ( "forceOpen", Type "Bool" [] ), ( "today", Type "Date" [] ), ( "currentMonth", Type "Date" [] ), ( "currentDates", Type "List" ([ Type "Date" [] ]) ), ( "pickedDate", Type "Maybe" ([ Type "Date" [] ]) ), ( "settings", Type "Settings" [] ) ]) Nothing ), ( "Settings", Record ([ ( "placeholder", Type "String" [] ), ( "classNamespace", Type "String" [] ), ( "inputClassList", Type "List" ([ Tuple ([ Type "String" [], Type "Bool" [] ]) ]) ), ( "inputName", Type "Maybe" ([ Type "String" [] ]) ), ( "inputId", Type "Maybe" ([ Type "String" [] ]) ), ( "isDisabled", Lambda (Type "Date" []) (Type "Bool" []) ), ( "parser", Lambda (Type "String" []) (Type "Result" ([ Type "String" [], Type "Date" [] ])) ), ( "dateFormatter", Lambda (Type "Date" []) (Type "String" []) ), ( "dayFormatter", Lambda (Type "Day" []) (Type "String" []) ), ( "monthFormatter", Lambda (Type "Month" []) (Type "String" []) ), ( "yearFormatter", Lambda (Type "Int" []) (Type "String" []) ), ( "cellFormatter", Lambda (Type "String" []) (Type "Html" ([ Type "Msg" [] ])) ), ( "firstDayOfWeek", Type "Day" [] ), ( "pickedDate", Type "Maybe" ([ Type "Date" [] ]) ) ]) Nothing ) ], externalNamesModuleInfo = Dict.fromList [ ( "Date", { dottedModulePath = "Date", name = "Date" } ), ( "Day", { dottedModulePath = "Date", name = "Day" } ), ( "Maybe", { dottedModulePath = "Maybe", name = "Maybe" } ), ( "Month", { dottedModulePath = "Date", name = "Month" } ), ( "Result", { dottedModulePath = "Result", name = "Result" } ) ], viewFunctions = Dict.fromList [], dottedModulePath = "DatePicker" } )
        , ( "Dialog", { localUnionTypes = Dict.fromList [ ( "ActionType", { name = "ActionType", typeVars = [], definition = [ ( "Main", [] ), ( "Destructive", [] ) ] } ), ( "DialogResult", { name = "DialogResult", typeVars = [ "a" ], definition = [ ( "KeepOpen", [ Var "a" ] ), ( "CloseDialog", [] ) ] } ) ], localTypeAliases = Dict.fromList [ ( "Configuration", Record ([ ( "title", Type "String" [] ), ( "automationId", Type "String" [] ), ( "mainAction", Record ([ ( "label", Type "String" [] ), ( "submittingLabel", Type "String" [] ), ( "title", Type "String" [] ), ( "actionType", Type "ActionType" [] ), ( "msg", Var "msg" ) ]) Nothing ), ( "content", Type "List" ([ Type "Html" ([ Var "msg" ]) ]) ), ( "closeMsg", Var "msg" ), ( "onClickMsg", Var "msg" ) ]) Nothing ), ( "State", Record ([ ( "ongoingAction", Type "Bool" [] ) ]) Nothing ) ], externalNamesModuleInfo = Dict.fromList [], viewFunctions = Dict.fromList [], dottedModulePath = "Dialog" } )
        , ( "Maybe", { localUnionTypes = Dict.fromList [ ( "Maybe", { name = "Maybe", typeVars = [ "a" ], definition = [ ( "Just", [ Var "a" ] ), ( "Nothing", [] ) ] } ) ], localTypeAliases = Dict.fromList [], externalNamesModuleInfo = Dict.fromList [], viewFunctions = Dict.fromList [], dottedModulePath = "Maybe" } )
        , ( "Result", { localUnionTypes = Dict.fromList [], localTypeAliases = Dict.fromList [], externalNamesModuleInfo = Dict.fromList [ ( "Result", { dottedModulePath = "Result", name = "Result" } ) ], viewFunctions = Dict.fromList [], dottedModulePath = "Result" } )
        ]


subjectModuleInfo =
    { localUnionTypes = Dict.fromList [ ( "DateField", { name = "DateField", typeVars = [], definition = [ ( "ResponsesDue", [] ), ( "InterviewDue", [] ) ] } ), ( "Msg", { name = "Msg", typeVars = [], definition = [ ( "OpenPicker", [ Type "DateField" [] ] ), ( "DatePickerMsg", [ Type "DateField" [], Type "DatePicker.Msg" [] ] ), ( "ClosePickers", [] ), ( "NoOp", [] ) ] } ) ], localTypeAliases = Dict.fromList [ ( "Configuration", Record ([ ( "mainMsg", Lambda (Type "String" []) (Lambda (Type "Date" []) (Lambda (Type "Date" []) (Var "msg"))) ), ( "mappingMsg", Lambda (Type "Msg" []) (Var "msg") ), ( "closeMsg", Var "msg" ), ( "headerContent", Type "List" ([ Type "Html" ([ Var "msg" ]) ]) ) ]) Nothing ), ( "State", Record ([ ( "responses", Type "Date" [] ), ( "interview", Type "Date" [] ), ( "currentDate", Type "Maybe" ([ Type "Date" [] ]) ), ( "responsesPicker", Type "Maybe" ([ Type "DatePicker.DatePicker" [] ]) ), ( "interviewPicker", Type "Maybe" ([ Type "DatePicker.DatePicker" [] ]) ), ( "path", Type "String" [] ), ( "subject", Type "String" [] ), ( "dialogState", Type "Dialog.State" [] ) ]) Nothing ) ], viewFunctions = Dict.fromList [ ( "dateView", Lambda (Type "Maybe" ([ Type "DatePicker.DatePicker" [] ])) (Lambda (Type "Maybe" ([ Type "Date" [] ])) (Lambda (Type "Date" []) (Lambda (Type "DateField" []) (Type "Html" ([ Type "Msg" [] ]))))) ), ( "displayView", Lambda (Type "Maybe" ([ Type "Date" [] ])) (Lambda (Type "Date" []) (Lambda (Type "DateField" []) (Type "Html" ([ Type "Msg" [] ])))) ), ( "pickerView", Lambda (Type "DatePicker.DatePicker" []) (Lambda (Type "DateField" []) (Type "Html" ([ Type "Msg" [] ]))) ), ( "view", Lambda (Type "Configuration" ([ Var "msg" ])) (Lambda (Type "State" []) (Type "Html" ([ Var "msg" ]))) ) ], externalNamesModuleInfo = Dict.fromList [ ( "Date", { dottedModulePath = "Date", name = "Date" } ), ( "DatePicker.DatePicker", { dottedModulePath = "DatePicker", name = "DatePicker" } ), ( "DatePicker.Msg", { dottedModulePath = "DatePicker", name = "Msg" } ), ( "Dialog.State", { dottedModulePath = "Dialog", name = "State" } ), ( "Maybe", { dottedModulePath = "Maybe", name = "Maybe" } ) ], dottedModulePath = "Components.LaunchDialog.LaunchDialog" }


qualifiedAllTypes =
    qualifyAllTypes { allModulesInfo = moduleInfos, subjectModuleInfo = subjectModuleInfo }


suite : Test
suite =
    describe "DataGeneration.elm"
        [ describe "generateViewFunction"
            [ test "dateView" <|
                \_ ->
                    (generateViewFunction
                        qualifiedAllTypes
                        -- ( "dateView", (qualifiedAllTypes.subjectModuleInfo.viewFunctions |> unsafeDictGet "" "dateView") )
                        -- ( "displayView", (qualifiedAllTypes.subjectModuleInfo.viewFunctions |> unsafeDictGet "" "displayView") )
                        -- ( "pickerView", (qualifiedAllTypes.subjectModuleInfo.viewFunctions |> unsafeDictGet "" "pickerView") )
                        "Components.LaunchDialog.LaunchDialog"
                        ( "view", (qualifiedAllTypes.subjectModuleInfo.viewFunctions |> unsafeDictGet "" "view") )
                    )
                        |> Expect.equal
                            "Just  ( 1 ) "
            ]
        ]
