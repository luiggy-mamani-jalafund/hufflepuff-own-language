{-# OPTIONS_GHC -Wno-partial-fields #-}
module AbstractSyntaxTree (module AbstractSyntaxTree) where

-- TERMINALS
data Literal
  = LStringIdentifier StringIdentifier
  | LString StringFree
  | LTakeTaskAttribute TakeTaskAttributeLiteral
  | LTakeMemberAttribute TakeMemberAttribute
  deriving (Show, Eq)

newtype StringIdentifier = StringId String deriving (Show, Eq)

newtype StringFree = String String deriving (Show, Eq)

type Identifier = String

data Type
  = TStringId
  | TString
  | TState
  | TBool
  | TMember
  | TTag
  | TTask
  | TListTask
  | TListList
  | TListStringId
  | TListString
  | TListState
  | TListBool
  | TListMember
  | TListTag
  deriving (Show, Read, Eq)

data Value
  = ValLiteral Literal
  | ValTask Task
  | ValTag Tag
  | ValMember Member
  | ValList List
  | ValBool Bool
  deriving (Show, Eq)

-- TASK DATA
data Tag = Tag StringIdentifier | NoTag deriving (Show, Eq)

type TaskState = StringIdentifier

data Task = Task
  { title :: TitleTask,
    description :: DescriptionTask,
    state :: StateTask,
    members :: MembersTask,
    tag :: TagTask,
    subTasks :: SubTasksTask
  }
  deriving (Show, Eq)

-- TASK ATTRIBUTTE
data TitleTask
  = TaskValueTitle StringFree
  | TaskIdentifierTitle Identifier
  | TaskTakeTitle Identifier
  deriving (Show, Eq)

data DescriptionTask
  = TaskValueDescription StringFree
  | TaskIdentifierDescription Identifier
  | TaskTakeDescription Identifier
  deriving (Show, Eq)

data StateTask
  = TaskValueState TaskState
  | TaskIdentifierState Identifier
  | TaskTakeState Identifier
  deriving (Show, Eq)

data TagTask
  = TaskValueTag Tag
  | TaskIdentifierTag Identifier
  | TaskTakeTag Identifier
  deriving (Show, Eq)

data MembersTask
  = TaskValueMembers List
  | TaskIdentifierMembers Identifier
  | TaskTakeMembers Identifier
  deriving (Show, Eq)

data SubTasksTask
  = TaskValueSubTasks List
  | TaskIdentifierSubTasks Identifier
  | TaskTakeSubTasks Identifier
  deriving (Show, Eq)

-- TAKE TASK ATTRIBUTE
data TakeTaskAttribute
  = TakeTaskAttributeStrings TakeTaskAttributeLiteral
  | TakeTaskAttributeMembers Identifier
  | TakeTaskAttributeSubTasks Identifier
  deriving (Show, Eq)

data TakeTaskAttributeLiteral
  = TakeTaskAttributeTitle Identifier
  | TakeTaskAttributeDescription Identifier
  | TakeTaskAttributeState Identifier
  | TakeTaskAttributeTag Identifier
  deriving (Show, Eq)

-- MEMBER DATA
type Role = StringIdentifier

type Name = StringFree

data Member
  = Member
      { name :: MemberName,
        role :: MemberRole
      }
  | NoAssigned
  deriving (Show, Eq)

data MemberName
  = MemberValueName Name
  | MemberIdentifierName Identifier
  | MemberTakeName Identifier
  deriving (Show, Eq)

data MemberRole
  = MemberValueRole Role
  | MemberIdentifierRole Identifier
  | MemberTakeRole Identifier
  deriving (Show, Eq)

data TakeMemberAttribute
  = TakeMemberAttributeName Identifier
  | TakeMemberAttributeRole Identifier
  deriving (Show, Eq)

-- LIST DATA
data List
  = ListStringId [StringIdentifier]
  | ListString [StringFree]
  | ListBool [Bool]
  | ListTask [Task]
  | ListTag [Tag]
  | ListState [TaskState]
  | ListMember [Member]
  | ListList [List]
  deriving (Show, Eq)

-- FUNCTION
data Func = Func Identifier Type [FuncParam] FuncBody deriving (Show, Eq)

data FuncParam = FuncParam Identifier Type deriving (Show, Read, Eq)

data FuncBody
  = FuncReturn Statement
  | FuncPattern [PatternCase] PatternDefault
  deriving (Show, Eq)

data PatternCase = PatternCase [PatternCaseValue] Statement deriving (Show, Eq)

data PatternCaseValue
  = PatternCaseValue Value
  | PatternCaseEmpty
  deriving (Show, Eq)

newtype PatternDefault = PatternDefault Statement deriving (Show, Eq)

-- FUNCTION CALL
data FuncCall = FuncCall Identifier [FuncCallParam] deriving (Show, Eq)

data FuncCallParam
  = FuncCallParamValue Value
  | FuncCallParam FuncCall
  | FuncCallIdentifier Identifier
  deriving (Show, Eq)

-- BOOLEAN EXPRESSION
data BoolExpression
  = BoolValue Bool
  | BoolComparison Comparison
  deriving (Show, Eq)

data BoolComparator
  = Eq
  | Neq
  | Lt
  | Le
  | Gt
  | Ge
  | And
  | Or
  deriving (Show, Eq)

data Comparison
  = ComparisonString Literal BoolComparator Literal
  | ComparisonBool Bool BoolComparator Bool
  | ComparisonTask Task BoolComparator Task
  | ComparisonMember Member BoolComparator Member
  deriving (Show, Eq)

-- CONDITION STATEMENT
data Condition = Condition
  { ifCondition :: BoolExpression,
    thenStatement :: Statement,
    elseStatament :: Statement
  }
  deriving (Show, Eq)

-- CYCLE STATEMENT
type MapFunctionRef = Identifier

data Cycle = Cycle
  { mapF :: MapFunctionRef,
    mapL :: CycleList
  }
  deriving (Show, Eq)

data CycleList
  = CycleList List
  | CycleId Identifier
  deriving (Show, Eq)

-- STATEMENT
data Statement
  = SFuncCall FuncCall
  | SValue Value
  | STakeTaskAttribute TakeTaskAttribute
  | STakeMemberAttribute TakeMemberAttribute
  | SBoolExp BoolExpression
  | SBoolCondition Condition
  | SCycle Cycle
  deriving (Show, Eq)

-- DO NOTATION
newtype DoNotation
  = DoNotation [DoStatement]
  deriving (Show, Eq)

data DoStatement
  = DoAssignment Identifier Type Statement
  | DoPrint Print
  deriving (Show, Eq)

data Print
  = PrintRef Identifier
  | PrintStatement Statement
  deriving (Show, Eq)

-- CODE
data Code
  = Code [Func] DoNotation
  deriving (Show, Eq)
