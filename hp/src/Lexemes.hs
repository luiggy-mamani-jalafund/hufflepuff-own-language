module Lexemes where

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

data Strings
  = String Literal
  | TTAString TTAStrings
  | TMAString

data Value
  = ValString Strings
  | ValBool Bool
  | ValTask Task
  | ValTag Tag
  | ValMember Member
  | ValList List

-- TASK DATA
data Tag = Tag Literal | NoTag

type State = Literal

data Task = Task
  { title :: TitleTask,
    description :: DescriptionTask,
    state :: StateTask,
    members :: MembersTask,
    tag :: TagTask,
    subTasks :: SubTasksTask
  }

-- TASK ATTRIBUTTE
data TitleTask
  = TVTitle Literal
  | TITitle Identifier

data DescriptionTask
  = TVDescription Literal
  | TIDescription Identifier

data StateTask
  = TVState State
  | TIState Identifier

data TagTask
  = TVTag Tag
  | TITag Identifier

data MembersTask
  = TMembersValue List
  | TMembersId Identifier

data SubTasksTask
  = TSubTasksValue List
  | TSubTasksId Identifier

-- TAKE TASK ATTRIBUTE
data TakeTaskAttribute
  = TTAStrings TTAStrings
  | TTAMembers Identifier
  | TTASubTasks Identifier

data TTAStrings
  = TTATitle Identifier
  | TTADescription Identifier
  | TTAState Identifier
  | TTATag Identifier

-- MEMBER DATA
type Role = Literal

type Name = Literal

data Member
  = Member Name Role
  | NoAssigned

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

-- FUNCTION
data Func = Func Identifier Type [FunParam] FuncBody

data FunParam = FParam Identifier Type

data FuncBody
  = FReturn Statement
  | FPattern Pattern

data Pattern = Pattern [PatternCase] PatternDefault

data PatternCase = Case [PatternCaseValue] Statement

data PatternCaseValue
  = PCaseValue Value
  | PCaseEmpty

newtype PatternDefault = PDefault Statement

-- FUNCTION CALL
data FuncCall = FuncCall Identifier [FuncCallParam]

data FuncCallParam
  = FuncCallParamValue Value
  | FunCall FuncCall

-- BOOLEAN EXPRESSION
data BoolExpression
  = BExp Bool
  | BComparison Comparison

data BoolComparator
  = Eq
  | Neq
  | Lt
  | Le
  | Gt
  | Ge
  | And
  | Or

data Comparison
  = CString String BoolComparator String
  | CBool Bool BoolComparator Bool
  | CTask Task BoolComparator Task
  | CMember Member BoolComparator Member

-- CONDITION STATEMENT
type ConditionElse = Statement

type ConditionIf = Statement

data Condition = Condition BoolExpression ConditionIf ConditionElse

-- CYCLE STATEMENT
type MapFunctionRef = Identifier

data Cycle = Cycle MapFunctionRef CycleList

data CycleList
  = CycleList List
  | CycleId Identifier

-- STATEMENT
data Statement
  = SFuncCall FuncCall
  | SValue Value
  | STTA TakeTaskAttribute
  | SCondition Condition
  | SCycle Cycle

-- START
data Code = Code [Func] DoStatement

newtype DoStatement = DoStatement FuncCall