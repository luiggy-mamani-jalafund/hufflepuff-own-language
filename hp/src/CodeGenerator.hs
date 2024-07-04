{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module CodeGenerator where

import AbstractSyntaxTree
import Data.List (intercalate)
import Data.Char


generateLiteral :: Literal -> String
generateLiteral literal = ""

generateStringIdentifier :: StringIdentifier -> String
generateStringIdentifier (StringId id) = "\"" ++ id ++ "\""

generateIdentifier :: Identifier -> String
generateIdentifier id = "\"" ++ id ++ "\""

generateStringFree :: StringFree -> String
generateStringFree (String str) = "\"" ++ str ++ "\""

generateType :: Type -> String
generateType TStringId = "\"StringId\""
generateType TString = "\"String\""
generateType TState = "\"State\""
generateType TBool = "\"Bool\""
generateType TMember = "\"Member\""
generateType TTag = "\"Tag\""
generateType TTask = "\"Task\""
generateType TListTask = "\"ListTask\""
generateType TListList = "\"ListList\""
generateType TListStringId = "\"ListStringId\""
generateType TListString = "\"ListString\""
generateType TListState = "\"ListState\""
generateType TListBool = "\"ListBool\""
generateType TListMember = "\"ListMember\""
generateType TListTag = "\"ListTag\""

generateValue :: Value -> String
generateValue (ValLiteral literal) = generateLiteral literal
generateValue (ValTask task) = generateTask task
generateValue (ValTag tag) = generateTag tag
generateValue (ValMember member) = generateMember member
generateValue (ValList list) = generateList list
generateValue (ValBool boolVal) = show boolVal

generateTag :: Tag -> String
generateTag (Tag id) = generateStringIdentifier id
generateTag NoTag = "null"

generateTask :: Task -> String
generateTask (Task title description state members tag subTasks) = 
    "new Task(" ++ generateTitleTask title ++ ", " ++ generateDescriptionTask description ++ ", " ++
    generateStateTask state ++ ", " ++ generateMembersTask members ++ ", " ++ generateTagTask tag ++ 
    ", " ++ generateSubTasksTask subTasks ++ ")"  

generateTitleTask :: TitleTask -> String
generateTitleTask (TaskValueTitle strFree) = generateStringFree strFree
generateTitleTask (TaskIdentifierTitle id) = generateIdentifier id
generateTitleTask (TaskTakeTitle id) = generateIdentifier id 

generateDescriptionTask :: DescriptionTask -> String
generateDescriptionTask (TaskValueDescription strFree) = generateStringFree strFree
generateDescriptionTask (TaskIdentifierDescription id) = generateIdentifier id
generateDescriptionTask (TaskTakeDescription id) = generateIdentifier id

generateStateTask :: StateTask -> String
generateStateTask (TaskValueState taskState) = generateStringIdentifier taskState
generateStateTask (TaskIdentifierState id) = generateIdentifier id
generateStateTask (TaskTakeState id) = generateIdentifier id

generateMembersTask :: MembersTask -> String
generateMembersTask (TaskValueMembers taskList) = generateList taskList
generateMembersTask (TaskIdentifierMembers id) = generateIdentifier id
generateMembersTask (TaskTakeMembers id) = generateIdentifier id

generateTagTask :: TagTask -> String
generateTagTask (TaskValueTag taskValueTag) = generateTag taskValueTag
generateTagTask (TaskIdentifierTag id) = generateIdentifier id
generateTagTask (TaskTakeTag id) = generateIdentifier id

generateSubTasksTask :: SubTasksTask -> String
generateSubTasksTask (TaskValueSubTasks taskValueSubTask) = generateList taskValueSubTask
generateSubTasksTask (TaskIdentifierSubTasks id) = generateIdentifier id
generateSubTasksTask (TaskTakeSubTasks id) = generateIdentifier id

generateTakeTaskAttribute :: TakeTaskAttribute -> String
generateTakeTaskAttribute (TakeTaskAttributeStrings takeAttributeLit) = generateTakeTaskAttributeLiteral takeAttributeLit
generateTakeTaskAttribute (TakeTaskAttributeMembers id) = generateIdentifier id
generateTakeTaskAttribute (TakeTaskAttributeSubTasks id) = generateIdentifier id

generateTakeTaskAttributeLiteral :: TakeTaskAttributeLiteral -> String
generateTakeTaskAttributeLiteral (TakeTaskAttributeTitle id) = generateIdentifier id
generateTakeTaskAttributeLiteral (TakeTaskAttributeDescription id) = generateIdentifier id
generateTakeTaskAttributeLiteral (TakeTaskAttributeState id) = generateIdentifier id
generateTakeTaskAttributeLiteral (TakeTaskAttributeTag id) = generateIdentifier id

generateMember :: Member -> String
generateMember (Member name role) = ""

generateMemberName :: MemberName -> String
generateMemberName (MemberValueName memberName) = ""
generateMemberName (MemberIdentifierName id) = ""
generateMemberName (MemberTakeName id) = ""

generateMemberRole :: MemberRole -> String
generateMemberRole (MemberValueRole memberRole) = ""
generateMemberRole (MemberIdentifierRole id) = ""
generateMemberRole (MemberTakeRole id) = ""

generateTakeMemberAttribute :: TakeMemberAttribute -> String
generateTakeMemberAttribute tma = ""

generateList :: List -> String
generateList (ListStringId ids) = generateStringIdList ids
generateList (ListString strs) = generateStringList strs
generateList (ListBool bools) = generateBoolList bools
generateList (ListTask tasks) = generateTaskList tasks
generateList (ListTag tags) = generateTagList tags 
generateList (ListState states) = generateStateList states
generateList (ListMember memebers) = generateMemberList memebers
generateList (ListList lists) = generateListList lists

generateStringIdList :: [StringIdentifier] -> String 
generateStringIdList ids = "[" ++ intercalate ", " (map generateStringIdentifier ids) ++ "]"

generateStringList :: [StringFree] -> String
generateStringList strs = "[" ++ intercalate ", " (map generateStringFree strs) ++ "]"

generateBoolList :: [Bool] -> String
generateBoolList bools = "[" ++ intercalate ", " (map show bools) ++ "]"

generateTaskList :: [Task] -> String
generateTaskList tasks = "[" ++ intercalate ", " (map generateTask tasks) ++ "]"

generateTagList :: [Tag] -> String
generateTagList tags = "[" ++ intercalate ", " (map generateTag tags) ++ "]"

generateStateList :: [TaskState] -> String 
generateStateList states = "[" ++ intercalate ", " (map generateStringIdentifier states) ++ "]"

generateMemberList :: [Member] -> String
generateMemberList members = "[" ++ intercalate ", " (map generateMember members) ++ "]"

generateListList :: [List] -> String
generateListList lists = "[" ++ intercalate ", " (map generateList lists) ++ "]"

generateFuncs :: [Func] -> String
generateFuncs = foldl (\acc f -> acc ++ generateFunc f ++ "\n\n") ""

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
generateFuncBody (FuncPattern [] (PatternDefault statement)) _ =
  generateStatement statement
generateFuncBody (FuncPattern cases caseDef) paramsIds =
  "{ \n\t"
    ++ generatePatternCases cases paramsIds ""
    ++ generatePatternDefault caseDef
    ++ "\n}"

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

generateBoolValue :: Bool -> [Char]
generateBoolValue b = let str = show b in toLower (head str) : tail str

generateBoolExpression :: BoolExpression -> String
generateBoolExpression (BoolValue b) = generateBoolValue b
generateBoolExpression (BoolComparison c) = generateComparison c

generateBoolComparator :: BoolComparator -> String
generateBoolComparator Eq  = "=="
generateBoolComparator Neq = "!=="
generateBoolComparator Lt = "<"
generateBoolComparator Le = "<="
generateBoolComparator Gt = ">"
generateBoolComparator Ge = ">="
generateBoolComparator And = "&&"
generateBoolComparator Or = "||"

comparisonTemplate :: String -> BoolComparator -> String -> String
comparisonTemplate s1 cmp s2 = s1 ++ " " ++ generateBoolComparator cmp ++ " " ++ s2

generateComparison :: Comparison -> String
generateComparison (ComparisonBool s1 cmp s2) =
  comparisonTemplate (generateBoolValue s1) cmp (generateBoolValue s2)
generateComparison (ComparisonString s1 cmp s2) =
  comparisonTemplate (generateLiteral s1) cmp (generateLiteral s2)
generateComparison (ComparisonTask s1 cmp s2) =
  comparisonTemplate (generateTask s1) cmp (generateTask s2)
generateComparison (ComparisonMember s1 cmp s2) =
  comparisonTemplate (generateMember s1) cmp (generateMember s2)

generateCondition :: Condition -> String
generateCondition (Condition ifCond thenStat elseStat) =
  "("
    ++ generateBoolExpression ifCond
    ++ " ? "
    ++ generateStatement thenStat
    ++ " : "
    ++ generateStatement elseStat
    ++ ")"

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
