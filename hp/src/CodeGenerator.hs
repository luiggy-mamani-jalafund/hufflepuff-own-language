module CodeGenerator where

import AbstractSyntaxTree

generateLiteral :: Literal -> String
generateLiteral literal = ""

generateStringIdentifier :: StringIdentifier -> String
generateStringIdentifier si = ""

generateStringFree :: StringFree -> String
generateStringFree sf = ""

generateType :: Type -> String
generateType t = ""

generateValue :: Value -> String
generateValue v = ""

generateTag :: Tag -> String
generateTag t = ""

generateTask :: Task -> String
generateTask task = ""

generateTitleTask :: TitleTask -> String
generateTitleTask tt = ""

generateDescriptionTask :: DescriptionTask -> String
generateDescriptionTask dt = ""

generateStateTask :: StateTask -> String
generateStateTask st = ""

generateTagTask :: TagTask -> String
generateTagTask tt = ""

generateMembersTask :: MembersTask -> String
generateMembersTask mt = ""

generateSubTasksTask :: SubTasksTask -> String
generateSubTasksTask st = ""

generateTakeTaskAttribute :: TakeTaskAttribute -> String
generateTakeTaskAttribute tta = ""

generateTakeTaskAttributeLiteral :: TakeTaskAttributeLiteral -> String
generateTakeTaskAttributeLiteral ttal = ""

generateMember :: Member -> String
generateMember m = ""

generateMemberName :: MemberName -> String
generateMemberName mn = ""

generateMemberRole :: MemberRole -> String
generateMemberRole mr = ""

generateTakeMemberAttribute :: TakeMemberAttribute -> String
generateTakeMemberAttribute tma = ""

generateList :: List -> String
generateList l = ""

generateFunc :: Func -> String
generateFunc f = ""

generateFuncParam :: FuncParam -> String
generateFuncParam fp = ""

generateFuncBody :: FuncBody -> String
generateFuncBody fb = ""

generatePatternCase :: PatternCase -> String -> [String] -> String
generatePatternCase (PatternCase patternVals statement) condition paramsIds =
  condition
    ++ " ("
    ++ generatePatternCase' "" patternVals paramsIds
    ++ ") { return "
    ++ generateStatement statement
    ++ "}"

generatePatternCase' :: String -> [PatternCaseValue] -> [String] -> String
generatePatternCase' acc [] [] = acc
generatePatternCase' acc [x] [y] = acc ++ generatePatternCaseValue x y
generatePatternCase' acc (x : xs) (y : ys) =
  let concatCond = if not (null xs) && not (isEmptyPatternCase x) then " && " else ""
   in generatePatternCase' (acc ++ generatePatternCaseValue x y ++ concatCond) xs ys
generatePatternCase' acc _ [] = acc
generatePatternCase' acc [] _ = acc

isEmptyPatternCase :: PatternCaseValue -> Bool
isEmptyPatternCase PatternCaseEmpty = True
isEmptyPatternCase _ = False

generatePatternCaseValue :: PatternCaseValue -> String -> String
generatePatternCaseValue (PatternCaseValue v) cv = cv ++ "===" ++ generateValue v
generatePatternCaseValue PatternCaseEmpty _ = ""

generatePatternDefault :: PatternDefault -> String
generatePatternDefault pd = ""

generateFuncCall :: FuncCall -> String
generateFuncCall fc = ""

generateFuncCallParam :: FuncCallParam -> String
generateFuncCallParam fcp = ""

generateBoolExpression :: BoolExpression -> String
generateBoolExpression be = ""

generateBoolComparator :: BoolComparator -> String
generateBoolComparator bc = ""

generateComparison :: Comparison -> String
generateComparison c = ""

generateCondition :: Condition -> String
generateCondition c = ""

generateCycle :: Cycle -> String
generateCycle cy = ""

generateCycleList :: CycleList -> String
generateCycleList cl = ""

generateStatement :: Statement -> String
generateStatement s = ""

generateDoNotation :: DoNotation -> String
generateDoNotation dn = ""

generateDoStatement :: DoStatement -> String
generateDoStatement ds = ""

generatePrint :: Print -> String
generatePrint p = ""

-- This have to generate the Task, Member classes, the main function and its call
generateCode :: Code -> String
generateCode c = ""
