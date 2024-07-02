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
generateFunc (Func identifier _ params body) =
  "const "
    ++ identifier
    ++ " = ("
    ++ generateFuncParams params ""
    ++ ") => \n"
    ++ generateFuncBody body (map (\(FuncParam ident _) -> ident) params)

generateFuncParams :: [FuncParam] -> String -> String
generateFuncParams [] acc = acc
generateFuncParams (x : xs) acc =
  let concatCond = if not (null xs) then ", " else ""
   in generateFuncParams xs (acc ++ generateFuncParam x ++ concatCond)

generateFuncParam :: FuncParam -> String
generateFuncParam (FuncParam identifier _) = identifier

generateFuncBody :: FuncBody -> [String] -> String
generateFuncBody (FuncReturn statement) _ = generateStatement statement
generateFuncBody (FuncPattern cases caseDef) paramsIds =
  "{ \n\t" ++ generatePatternCases cases paramsIds "" ++ generatePatternDefault caseDef ++ "\n}"

generatePatternCases :: [PatternCase] -> [String] -> String -> String
generatePatternCases [] _ acc = acc
generatePatternCases (x : xs) paramsIds acc =
  let concatCond = if not (null xs) then " else " else " "
   in generatePatternCases xs paramsIds (acc ++ generatePatternCase x paramsIds ++ concatCond)

generatePatternCase :: PatternCase -> [String] -> String
generatePatternCase (PatternCase patternVals statement) paramsIds =
  "if ("
    ++ generatePatternCase' "" patternVals paramsIds
    ++ ") {\n\t\treturn "
    ++ generateStatement statement
    ++ "\n\t}"

generatePatternCase' :: String -> [PatternCaseValue] -> [String] -> String
generatePatternCase' acc [] [] = acc
generatePatternCase' acc [x] [y] = acc ++ generatePatternCaseValue x y
generatePatternCase' acc (x : xs) (y : ys) =
  let concatCond =
        if not (null xs)
          && not (isEmptyPatternCase x)
          && not (isEmptyPatternCase (head xs))
          then " && "
          else ""
   in generatePatternCase' (acc ++ generatePatternCaseValue x y ++ concatCond) xs ys
generatePatternCase' acc _ [] = acc
generatePatternCase' acc [] _ = acc

isEmptyPatternCase :: PatternCaseValue -> Bool
isEmptyPatternCase PatternCaseEmpty = True
isEmptyPatternCase _ = False

generatePatternCaseValue :: PatternCaseValue -> String -> String
generatePatternCaseValue (PatternCaseValue v) cv = cv ++ " === " ++ generateValue v
generatePatternCaseValue PatternCaseEmpty _ = ""

generatePatternDefault :: PatternDefault -> String
generatePatternDefault (PatternDefault statement) =
  " else {\n\t\treturn "
    ++ generateStatement statement
    ++ "\n\t}"

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
