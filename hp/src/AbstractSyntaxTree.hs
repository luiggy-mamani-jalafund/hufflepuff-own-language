module AbstractSyntaxTree (module AbstractSyntaxTree) where

-- TERMINALS
type Literal = String

type Identifier = String

data Type
  = TStringId
  | TStringIdSpace
  | TStringParagraph
  | TState
  | TBool
  | TMember
  | TTag
  | TTask
  | TListTask
  | TListList
  | TListStringId
  | TListStringIdSpace
  | TListStringParagraph
  | TListState
  | TListBool
  | TListMember
  | TListTag
  deriving (Show, Read)

data Strings
  = String Literal
  | TTAString TTAStrings
  | TMAString TakeMemberAttribute
  deriving (Show)

data Value
  = ValString Strings
  | ValBool Bool
  | ValTask Task
  | ValTag Tag
  | ValMember Member
  | ValList List
  deriving (Show)

-- TASK DATA
data Tag = Tag Literal | NoTag deriving (Show)

type State = Literal

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
  = TVTitle Literal
  | TITitle Identifier
  deriving (Show)

data DescriptionTask
  = TVDescription Literal
  | TIDescription Identifier
  deriving (Show)

data StateTask
  = TVState State
  | TIState Identifier
  deriving (Show)

data TagTask
  = TVTag Tag
  | TITag Identifier
  deriving (Show)

data MembersTask
  = TMembersValue List
  | TMembersId Identifier
  deriving (Show)

data SubTasksTask
  = TSubTasksValue List
  | TSubTasksId Identifier
  deriving (Show)

-- TAKE TASK ATTRIBUTE
data TakeTaskAttribute
  = TTAStrings TTAStrings
  | TTAMembers Identifier
  | TTASubTasks Identifier
  deriving (Show)

data TTAStrings
  = TTATitle Identifier
  | TTADescription Identifier
  | TTAState Identifier
  | TTATag Identifier
  deriving (Show)

-- MEMBER DATA
type Role = Literal

type Name = Literal

data Member
  = Member MemberName MemberRole
  | NoAssigned
  deriving (Show)

data MemberName
  = MVName Literal
  | MIName Identifier
  deriving (Show)

data MemberRole
  = MVRole Literal
  | MIRole Identifier
  deriving (Show)

data TakeMemberAttribute
  = TMAName Identifier
  | TMARole Identifier
  deriving (Show)

-- LIST DATA
data List
  = ListStringId [Identifier]
  | ListStringIdSpace [Literal]
  | ListStringParagraph [Literal]
  | ListBool [Bool]
  | ListTask [Task]
  | ListTag [Tag]
  | ListState [State]
  | ListMember [Member]
  | ListList [List]
  deriving (Show)

-- FUNCTION
data Func = Func Identifier Type [FunParam] FuncBody deriving (Show)

data FunParam = FunParam Identifier Type deriving (Show, Read)

data FuncBody
  = FuncReturn Statement
  | FuncPattern Pattern
  deriving (Show)

data Pattern = Pattern [PatternCase] PatternDefault deriving (Show)

data PatternCase = Case [PatternCaseValue] Statement deriving (Show)

data PatternCaseValue
  = PCaseValue Value
  | PCaseEmpty
  deriving (Show)

newtype PatternDefault = PDefault Statement deriving (Show)

-- FUNCTION CALL
data FuncCall = FuncCall Identifier [FuncCallParam] deriving (Show)

data FuncCallParam
  = FuncCallParamValue Value
  | FunCall FuncCall
  deriving (Show)

-- BOOLEAN EXPRESSION
data BoolExpression
  = BExp Bool
  | BComparison Comparison
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
  = CString String BoolComparator String
  | CBool Bool BoolComparator Bool
  | CTask Task BoolComparator Task
  | CMember Member BoolComparator Member
  deriving (Show)

-- CONDITION STATEMENT
type ConditionElse = Statement

type ConditionIf = Statement

data Condition = Condition BoolExpression ConditionIf ConditionElse deriving (Show)

-- CYCLE STATEMENT
type MapFunctionRef = Identifier

data Cycle = Cycle MapFunctionRef CycleList deriving (Show)

data CycleList
  = CycleList List
  | CycleId Identifier
  deriving (Show)

-- STATEMENT
data Statement
  = SFuncCall FuncCall
  | SValue Value
  | STTA TakeTaskAttribute
  | SCondition Condition
  | SCycle Cycle
  deriving (Show)

-- START
data Code = Code [Func] DoStatement deriving (Show)

newtype DoStatement = DoStatement FuncCall deriving (Show)