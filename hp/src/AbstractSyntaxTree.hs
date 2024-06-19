{-# OPTIONS_GHC -Wno-partial-fields #-}
module AbstractSyntaxTree (module AbstractSyntaxTree) where

-- TERMINALS
data Literal
  = LStringIdentifier StringIdentifier
  | LString StringFree
  | LTakeTaskAttribute TakeTaskAttributeLiteral
  | LTakeMemberAttribute TakeMemberAttribute
  deriving (Show)

newtype StringIdentifier = StringId String deriving (Show)

newtype StringFree = String String deriving (Show)

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
  deriving (Show, Read)

data Value
  = ValLiteral Literal
  | ValTask Task
  | ValTag Tag
  | ValMember Member
  | ValList List
  | ValBool Bool
  deriving (Show)

-- TASK DATA
data Tag = Tag StringIdentifier | NoTag deriving (Show)

type TaskState = StringIdentifier

data Task = Task
  { title :: TitleTask,
    description :: DescriptionTask,
    state :: StateTask,
    members :: MembersTask,
    tag :: TagTask,
    subTasks :: SubTasksTask
  }
  deriving (Show)

-- TASK ATTRIBUTTE
data TitleTask
  = TaskValueTitle StringFree
  | TaskIdentifierTitle Identifier
  | TaskTakeTitle Identifier
  deriving (Show)

data DescriptionTask
  = TaskValueDescription StringFree
  | TaskIdentifierDescription Identifier
  | TaskTakeDescription Identifier
  deriving (Show)

data StateTask
  = TaskValueState TaskState
  | TaskIdentifierState Identifier
  | TaskTakeState Identifier
  deriving (Show)

data TagTask
  = TaskValueTag Tag
  | TaskIdentifierTag Identifier
  | TaskTakeTag Identifier
  deriving (Show)

data MembersTask
  = TaskValueMembers List
  | TaskIdentifierMembers Identifier
  | TaskTakeMembers Identifier
  deriving (Show)

data SubTasksTask
  = TaskValueSubTasks List
  | TaskIdentifierSubTasks Identifier
  | TaskTakeSubTasks Identifier
  deriving (Show)

-- TAKE TASK ATTRIBUTE
data TakeTaskAttribute
  = TakeTaskAttributeStrings TakeTaskAttributeLiteral
  | TakeTaskAttributeMembers Identifier
  | TakeTaskAttributeSubTasks Identifier
  deriving (Show)

data TakeTaskAttributeLiteral
  = TakeTaskAttributeTitle Identifier
  | TakeTaskAttributeDescription Identifier
  | TakeTaskAttributeState Identifier
  | TakeTaskAttributeTag Identifier
  deriving (Show)

-- MEMBER DATA
type Role = StringIdentifier

type Name = StringFree

data Member
  = Member
      { name :: MemberName,
        role :: MemberRole
      }
  | NoAssigned
  deriving (Show)

data MemberName
  = MemberValueName Name
  | MemberIdentifierName Identifier
  deriving (Show)

data MemberRole
  = MemberValueRole Role
  | MemberIdentifierRole Identifier
  deriving (Show)

data TakeMemberAttribute
  = TakeMemberAttributeName Identifier
  | TakeMemberAttributeRole Identifier
  deriving (Show)

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
  deriving (Show)

-- FUNCTION
data Func = Func Identifier Type [FuncParam] FuncBody deriving (Show)

data FuncParam = FuncParam Identifier Type deriving (Show, Read)

data FuncBody
  = FuncReturn Statement
  | FuncPattern [PatternCase] PatternDefault
  deriving (Show)

data PatternCase = PatternCase [PatternCaseValue] Statement deriving (Show)

data PatternCaseValue
  = PatternCaseValue Value
  | PatternCaseEmpty
  deriving (Show)

newtype PatternDefault = PatternDefault Statement deriving (Show)

-- FUNCTION CALL
data FuncCall = FuncCall Identifier [FuncCallParam] deriving (Show)

data FuncCallParam
  = FuncCallParamValue Value
  | FuncCallParam FuncCall
  | FuncCallIdentifier Identifier
  deriving (Show)

-- BOOLEAN EXPRESSION
data BoolExpression
  = BoolValue Bool
  | BoolComparison Comparison
  deriving (Show)

data BoolComparator
  = Eq
  | Neq
  | Lt
  | Le
  | Gt
  | Ge
  | And
  | Or
  deriving (Show)

data Comparison
  = ComparisonString Literal BoolComparator Literal
  | ComparisonBool Bool BoolComparator Bool
  | ComparisonTask Task BoolComparator Task
  | ComparisonMember Member BoolComparator Member
  deriving (Show)

-- CONDITION STATEMENT

data Condition = Condition
  { ifCondition :: BoolExpression,
    thenStatement :: Statement,
    elseStatament :: Statement
  }
  deriving (Show)

-- CYCLE STATEMENT
type MapFunctionRef = Identifier

data Cycle = Cycle
  { mapF :: MapFunctionRef,
    mapL :: CycleList
  }
  deriving (Show)

data CycleList
  = CycleList List
  | CycleId Identifier
  deriving (Show)

-- STATEMENT
data Statement
  = SFuncCall FuncCall
  | SValue Value
  | STakeTaskAttribute TakeTaskAttribute
  | STakeMemberAttribute TakeMemberAttribute
  | SBoolExp BoolExpression
  | SBoolCondition Condition
  | SCycle Cycle
  deriving (Show)

-- DO NOTATION
newtype DoNotation
  = DoNotation [DoStatement]
  deriving (Show)

data DoStatement
  = DoAssignment Identifier Type Statement
  | DoPrint Print
  deriving (Show)

data Print
  = PrintRef Identifier
  | PrintStatement Statement
  deriving (Show)

-- CODE
data Code
  = Code [Func] DoNotation
  deriving (Show)
