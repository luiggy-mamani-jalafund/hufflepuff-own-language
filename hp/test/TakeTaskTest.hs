import Test.HUnit 
import src.HpParser

-- TakeTaskAttribute
testParseTakeTaskAttribute :: Test
testParseTakeTaskAttribute = TestCase $ do
    let input = "TakeTaskAttributeStrings (TakeTaskAttributeTitle \"taskTitle\")"
    case parse input of
        Right (TakeTaskAttributeStrings (TakeTaskAttributeTitle identifier)) ->
            assertEqual "Should parse TakeTaskAttribute correctly"
                        "taskTitle" identifier
        _ -> assertFailure "Failed to parse TakeTaskAttribute"

-- TakeTaskAttributeLiteral
testParseTakeTaskAttributeLiteral :: Test
testParseTakeTaskAttributeLiteral = TestCase $ do
    let input = "TakeTaskAttributeTitle \"taskTitle\""
    case parse input of
        Right (TakeTaskAttributeTitle identifier) ->
            assertEqual "Should parse TakeTaskAttributeLiteral correctly"
                        "taskTitle" identifier
        _ -> assertFailure "Failed to parse TakeTaskAttributeLiteral"