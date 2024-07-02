{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module CodeGenerator where

import AbstractSyntaxTree
import Data.List (intercalate)

generateLiteral :: Literal -> String
generateLiteral literal = ""

generateStringIdentifier :: StringIdentifier -> String
generateStringIdentifier (StringId id) = "\"" ++ id ++ "\""

generateIdentifier :: Identifier -> String
generateIdentifier id = "\"" ++ id ++ "\""

generateStringFree :: StringFree -> String
generateStringFree (String str) = "\"" ++ str ++ "\""

generateType :: Type -> String
generateType t = ""

generateValue :: Value -> String
generateValue v = ""

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
generateTakeTaskAttribute tta = ""

generateTakeTaskAttributeLiteral :: TakeTaskAttributeLiteral -> String
generateTakeTaskAttributeLiteral ttal = ""

generateMember :: Member -> String
generateMember (Member name role) = "new Member(" ++ generateMemberName name ++ ", " ++ generateMemberRole role ++ ")"
generateMember NoAssigned = "new Member('NoAssigned', 'No Role')"

generateMemberName :: MemberName -> String
generateMemberName (MemberValueName memberName) = generateStringFree memberName
generateMemberName (MemberIdentifierName id) = generateIdentifier id
generateMemberName (MemberTakeName id) = generateIdentifier id

generateMemberRole :: MemberRole -> String
generateMemberRole (MemberValueRole memberRole) = generateStringIdentifier memberRole
generateMemberRole (MemberIdentifierRole id) = generateIdentifier id
generateMemberRole (MemberTakeRole id) = generateIdentifier id

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

generateFunc :: Func -> String
generateFunc f = ""

generateFuncParam :: FuncParam -> String
generateFuncParam fp = ""

generateFuncBody :: FuncBody -> String
generateFuncBody fb = ""

generatePatternCase :: PatternCase -> String
generatePatternCase pc = ""

generatePatternCaseValue :: PatternCaseValue -> String
generatePatternCaseValue pcv = ""

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
