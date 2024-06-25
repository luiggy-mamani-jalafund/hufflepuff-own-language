import Test.HUnit 
import src.HpParser


-- LStringIdentifier
testParseLStringIdentifier :: Test
testParseLStringIdentifier = TestCase $ do
    let input = "LStringIdentifier (StringId \"identifier\")"
    case parse input of
        Right (LStringIdentifier (StringId str)) ->
            assertEqual "Should parse LStringIdentifier correctly"
                        "identifier" str
        _ -> assertFailure "Failed to parse LStringIdentifier"

-- LString
testParseLString :: Test
testParseLString = TestCase $ do
    let input = "LString (String \"free string\")"
    case parse input of
        Right (LString (String str)) ->
            assertEqual "Should parse LString correctly"
                        "free string" str
        _ -> assertFailure "Failed to parse LString"

-- Test para LTakeTaskAttribute
testParseLTakeTaskAttribute :: Test
testParseLTakeTaskAttribute = TestCase $ do
    let input = "LTakeTaskAttribute (TakeTaskAttributeTitle \"taskTitle\")"
    case parse input of
        Right (LTakeTaskAttribute (TakeTaskAttributeTitle identifier)) ->
            assertEqual "Should parse LTakeTaskAttribute correctly"
                        "taskTitle" identifier
        _ -> assertFailure "Failed to parse LTakeTaskAttribute"

-- Test para LTakeMemberAttribute
testParseLTakeMemberAttribute :: Test
testParseLTakeMemberAttribute = TestCase $ do
    let input = "LTakeMemberAttribute (TakeMemberAttribute \"memberId\")"
    case parse input of
        Right (LTakeMemberAttribute (TakeMemberAttribute identifier)) ->
            assertEqual "Should parse LTakeMemberAttribute correctly"
                        "memberId" identifier
        _ -> assertFailure "Failed to parse LTakeMemberAttribute"