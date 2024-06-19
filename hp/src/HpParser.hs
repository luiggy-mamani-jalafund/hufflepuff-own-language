module HpParser
  ( func,
    funcParam,
    funcParams,
    funcBody,
    task',
    taskMembers,
    member,
    statement,
    boolComparison,
    literal,
    takeTaskAttribute,
    casePattern,
    casePatternVal,
    casePatternVals,
    code,
    strFree,
  )
where

import AbstractSyntaxTree
import Lexer
import Text.Parsec
import Text.Parsec.String (Parser)

code :: Parser Code
code = do
  whiteSpace
  f <- funcs
  whiteSpace
  d <- doNotation
  whiteSpace
  return $ Code f d

funcs :: Parser [Func]
funcs = many func

func :: Parser Func
func = do
  whiteSpace
  reserved "func"
  whiteSpace
  funId <- identifier
  whiteSpace
  reservedOp "->"
  whiteSpace
  funType <- dataType
  whiteSpace
  _ <- string "{"
  whiteSpace
  reserved "params"
  whiteSpace
  _ <- string "{"
  whiteSpace
  params <- funcParams
  whiteSpace
  _ <- string "}"
  whiteSpace
  body <- funcBody
  whiteSpace
  _ <- string "}"
  whiteSpace
  return $ Func funId (str2type funType) params body

funcParam :: Parser FuncParam
funcParam = do
  whiteSpace
  i <- identifier
  whiteSpace
  reservedOp ":"
  whiteSpace
  paramType <- dataType
  whiteSpace
  return $ FuncParam i (str2type paramType)

funcParams :: Parser [FuncParam]
funcParams = sepBy funcParam (char ',')

funcBody :: Parser FuncBody
funcBody = try funcReturn <|> try funcPattern

funcReturn :: Parser FuncBody
funcReturn = do
  whiteSpace
  reserved "return"
  whiteSpace
  reservedOp "{"
  whiteSpace
  s <- statement
  whiteSpace
  reservedOp "}"
  return $ FuncReturn s

statement :: Parser Statement
statement =
  try (SBoolCondition <$> condition)
    <|> try (SBoolExp <$> boolExpression)
    <|> try (SValue <$> value')
    <|> try (SCycle <$> cycle')
    <|> try (SFuncCall <$> funcCall)
    <|> try (STakeTaskAttribute <$> takeTaskAttribute)
    <|> try (STakeMemberAttribute <$> takeMemberAttribute)

value' :: Parser Value
value' =
  try (ValLiteral <$> literal)
    <|> try (ValTask <$> task')
    <|> try (ValMember <$> member)
    <|> try (ValList <$> lists)
    <|> try (ValTag <$> tag')
    <|> try (ValBool <$> boolValue)

task' :: Parser Task
task' = do
  whiteSpace
  _ <- string "Task"
  whiteSpace
  _ <- string "{"

  whiteSpace
  _ <- string "title:"
  whiteSpace
  t <- taskTitle
  whiteSpace
  reservedOp ","

  whiteSpace
  _ <- string "description:"
  whiteSpace
  d <- taskDescription
  whiteSpace
  reservedOp ","

  whiteSpace
  _ <- string "state:"
  whiteSpace
  s <- taskState
  whiteSpace
  reservedOp ","

  whiteSpace
  _ <- string "members:"
  whiteSpace
  m <- taskMembers
  whiteSpace
  reservedOp ","

  whiteSpace
  _ <- string "tag:"
  whiteSpace
  tg <- taskTag
  whiteSpace
  reservedOp ","

  whiteSpace
  _ <- string "subTasks:"
  whiteSpace
  st <- taskSubTasks

  whiteSpace
  _ <- string "}"
  whiteSpace
  return $
    Task
      { title = t,
        description = d,
        state = s,
        members = m,
        tag = tg,
        subTasks = st
      }

taskTitle :: Parser TitleTask
taskTitle =
  try (TaskTakeTitle <$> takeTaskAttributeTitle)
    <|> try (TaskValueTitle . String <$> strFree)
    <|> try (TaskIdentifierTitle <$> identifier)

taskDescription :: Parser DescriptionTask
taskDescription =
  try (TaskTakeDescription <$> takeTaskAttributeDescription)
    <|> try (TaskValueDescription . String <$> strFree)
    <|> try (TaskIdentifierDescription <$> identifier)

taskState :: Parser StateTask
taskState =
  try (TaskTakeState <$> takeTaskAttributeState)
    <|> try (TaskValueState <$> state')
    <|> try (TaskIdentifierState <$> identifier)

taskMembers :: Parser MembersTask
taskMembers =
  try (TaskTakeMembers <$> takeTaskAttributeMembers)
    <|> try (TaskValueMembers <$> listOfMembers)
    <|> try
      ( TaskIdentifierMembers
          <$ whiteSpace
          <*> identifier
          <* whiteSpace
      )

taskTag :: Parser TagTask
taskTag =
  try (TaskTakeTag <$> takeTaskAttributeTag)
    <|> try (TaskValueTag <$> tag')
    <|> try (TaskIdentifierTag <$> identifier)

taskSubTasks :: Parser SubTasksTask
taskSubTasks =
  try (TaskTakeSubTasks <$> takeTaskAttributeSubTasks)
    <|> try (TaskValueSubTasks <$> lists)
    <|> try
      ( TaskIdentifierSubTasks
          <$ whiteSpace
          <*> identifier
          <* whiteSpace
      )

lists :: Parser List
lists =
  try listOfTasks
    <|> try listOfMembers
    <|> try listOfStates
    <|> try listOfTags
    <|> try listOfStrId
    <|> try listOfStrFree
    <|> try listOfBool
    <|> try listOfLists

listOfLists :: Parser List
listOfLists = do
  whiteSpace
  _ <- string "List:List"
  whiteSpace
  _ <- string "["
  whiteSpace
  i <- sepBy lists (char ',')
  whiteSpace
  _ <- string "]"
  whiteSpace
  return $ ListList i

listOfStrId :: Parser List
listOfStrId = do
  whiteSpace
  _ <- string "List:StringId"
  whiteSpace
  _ <- string "["
  whiteSpace
  i <- sepBy (StringId <$> strId) (char ',')
  whiteSpace
  _ <- string "]"
  whiteSpace
  return $ ListStringId i

listOfStrFree :: Parser List
listOfStrFree = do
  whiteSpace
  _ <- string "List:String"
  whiteSpace
  _ <- string "["
  whiteSpace
  i <- sepBy (String <$> strFree) (char ',')
  whiteSpace
  _ <- string "]"
  whiteSpace
  return $ ListString i

listOfMembers :: Parser List
listOfMembers = do
  whiteSpace
  _ <- string "List:Member"
  whiteSpace
  _ <- string "["
  whiteSpace
  i <- sepBy member (char ',')
  whiteSpace
  _ <- string "]"
  whiteSpace
  return $ ListMember i

listOfTasks :: Parser List
listOfTasks = do
  whiteSpace
  _ <- string "List:Task"
  whiteSpace
  _ <- string "["
  whiteSpace
  i <- sepBy task' (char ',')
  whiteSpace
  _ <- string "]"
  whiteSpace
  return $ ListTask i

listOfTags :: Parser List
listOfTags = do
  whiteSpace
  _ <- string "List:Tag"
  whiteSpace
  _ <- string "["
  whiteSpace
  i <- sepBy tag' (char ',')
  whiteSpace
  _ <- string "]"
  whiteSpace
  return $ ListTag i

listOfStates :: Parser List
listOfStates = do
  whiteSpace
  _ <- string "List:State"
  whiteSpace
  _ <- string "["
  whiteSpace
  i <- sepBy state' (char ',')
  whiteSpace
  _ <- string "]"
  whiteSpace
  return $ ListState i

listOfBool :: Parser List
listOfBool = do
  whiteSpace
  _ <- string "List:Bool"
  whiteSpace
  _ <- string "["
  whiteSpace
  i <- sepBy boolValue (char ',')
  whiteSpace
  _ <- string "]"
  whiteSpace
  return $ ListBool i

tag' :: Parser Tag
tag' =
  try $
    Tag . StringId <$> strId
      <|> do
        whiteSpace
        _ <- string "NoTag"
        whiteSpace
        return NoTag

state' :: Parser TaskState
state' = StringId <$> strId

member :: Parser Member
member =
  do
    whiteSpace
    _ <- string "Member"
    whiteSpace
    _ <- string "{"
    whiteSpace

    _ <- string "name: "
    whiteSpace
    n <- memberName
    whiteSpace
    reservedOp ","
    whiteSpace

    _ <- string "role: "
    whiteSpace
    r <- memberRole
    whiteSpace

    _ <- string "}"
    whiteSpace
    return $
      Member
        { name = n,
          role = r
        }
    <|> NoAssigned
      <$ whiteSpace
      <* string "NoAssigned"
      <* whiteSpace

takeMemberAttribute :: Parser TakeMemberAttribute
takeMemberAttribute =
  try tmaName
    <|> try tmaRole

tmaName :: Parser TakeMemberAttribute
tmaName = do
  whiteSpace
  i <- identifier
  _ <- string ".name"
  whiteSpace
  return $ TakeMemberAttributeName i

tmaRole :: Parser TakeMemberAttribute
tmaRole = do
  whiteSpace
  i <- identifier
  _ <- string ".role"
  whiteSpace
  return $ TakeMemberAttributeRole i

takeTaskAttribute :: Parser TakeTaskAttribute
takeTaskAttribute =
  try (TakeTaskAttributeStrings <$> takeTaskAttributeStrings)
    <|> try (TakeTaskAttributeMembers <$> takeTaskAttributeMembers)
    <|> try (TakeTaskAttributeSubTasks <$> takeTaskAttributeSubTasks)

takeTaskAttributeStrings :: Parser TakeTaskAttributeLiteral
takeTaskAttributeStrings =
  try (TakeTaskAttributeState <$> takeTaskAttributeState)
    <|> try (TakeTaskAttributeTitle <$> takeTaskAttributeTitle)
    <|> try (TakeTaskAttributeDescription <$> takeTaskAttributeDescription)
    <|> try (TakeTaskAttributeTag <$> takeTaskAttributeTag)

takeTaskAttributeTitle :: Parser String
takeTaskAttributeTitle = do
  whiteSpace
  i <- identifier
  _ <- string ".title"
  whiteSpace
  return i

takeTaskAttributeDescription :: Parser String
takeTaskAttributeDescription = do
  whiteSpace
  i <- identifier
  _ <- string ".description"
  whiteSpace
  return i

takeTaskAttributeState :: Parser String
takeTaskAttributeState = do
  whiteSpace
  i <- identifier
  _ <- string ".state"
  whiteSpace
  return i

takeTaskAttributeTag :: Parser String
takeTaskAttributeTag = do
  whiteSpace
  i <- identifier
  _ <- string ".tag"
  whiteSpace
  return i

takeTaskAttributeMembers :: Parser String
takeTaskAttributeMembers = do
  whiteSpace
  i <- identifier
  _ <- string ".members"
  whiteSpace
  return i

takeTaskAttributeSubTasks :: Parser String
takeTaskAttributeSubTasks = do
  whiteSpace
  i <- identifier
  _ <- string ".subTasks"
  whiteSpace
  return i

boolComparator :: Parser BoolComparator
boolComparator =
  Eq
    <$ whiteSpace
    <* string "=="
    <* whiteSpace
    <|> Neq
      <$ whiteSpace
      <* string "!="
      <* whiteSpace
    <|> Lt
      <$ whiteSpace
      <* string "<"
      <* whiteSpace
    <|> Le
      <$ whiteSpace
      <* string "<="
      <* whiteSpace
    <|> Gt
      <$ whiteSpace
      <* string ">"
      <* whiteSpace
    <|> Ge
      <$ whiteSpace
      <* string ">="
      <* whiteSpace
    <|> And
      <$ whiteSpace
      <* string "&&"
      <* whiteSpace
    <|> Or
      <$ whiteSpace
      <* string "||"
      <* whiteSpace

memberName :: Parser MemberName
memberName =
  MemberValueName . String <$> strFree
    <|> MemberIdentifierName <$> identifier

memberRole :: Parser MemberRole
memberRole =
  MemberValueRole . StringId <$> strId
    <|> MemberIdentifierRole <$> identifier

literal :: Parser Literal
literal =
  try (LString . String <$> strFree)
    <|> try (LStringIdentifier . StringId <$> strId)
    <|> try (LTakeTaskAttribute <$> takeTaskAttributeStrings)
    <|> try (LTakeMemberAttribute <$> takeMemberAttribute)

strId' :: Parser String
strId' = do
  l <- alphaNum
  v <- many (alphaNum <|> space)
  return $ l : v

strId :: Parser String
strId = do
  whiteSpace
  _ <- char '\"'
  v <- strId'
  _ <- char '\"'
  whiteSpace
  return v

strEmpty :: Parser String
strEmpty = do
  whiteSpace
  _ <- char '\"'
  _ <- char '\"'
  whiteSpace
  return ""

strFree :: Parser String
strFree = try strEmpty <|> try strFree'

strFree' :: Parser String
strFree' = do
  whiteSpace
  _ <- char '\"'
  v <- many (alphaNum <|> space <|> oneOf symbols)
  _ <- char '\"'
  whiteSpace
  return v

str2type :: String -> Type
str2type str = read $ 'T' : strType
  where
    strType = filter (/= ':') str

condition :: Parser Condition
condition = do
  reserved "if"
  whiteSpace
  _ <- string "("
  whiteSpace
  e <- boolExpression
  whiteSpace
  _ <- string ")"
  whiteSpace

  reserved "then"
  whiteSpace
  s1 <- statement
  whiteSpace
  reserved "else"
  whiteSpace
  s2 <- statement
  whiteSpace
  return $
    Condition
      { ifCondition = e,
        thenStatement = s1,
        elseStatament = s2
      }

boolExpression :: Parser BoolExpression
boolExpression =
  BoolComparison <$ whiteSpace <*> comparison <* whiteSpace
    <|> BoolValue <$ whiteSpace <*> boolValue <* whiteSpace

comparison :: Parser Comparison
comparison =
  try boolComparison
    <|> try strComparison
    <|> try taskComparison
    <|> try memberComparison

boolValue :: Parser Bool
boolValue =
  try (True <$ string "True")
    <|> try (False <$ string "False")

strComparison :: Parser Comparison
strComparison = do
  s1 <- literal
  whiteSpace
  cmp <- boolComparator
  whiteSpace
  s2 <- literal
  whiteSpace
  return $ ComparisonString s1 cmp s2

boolComparison :: Parser Comparison
boolComparison = do
  s1 <- boolValue
  whiteSpace
  cmp <- boolComparator
  whiteSpace
  s2 <- boolValue
  whiteSpace
  return $ ComparisonBool s1 cmp s2

taskComparison :: Parser Comparison
taskComparison = do
  s1 <- task'
  whiteSpace
  cmp <- boolComparator
  whiteSpace
  s2 <- task'
  whiteSpace
  return $ ComparisonTask s1 cmp s2

memberComparison :: Parser Comparison
memberComparison = do
  s1 <- member
  whiteSpace
  cmp <- boolComparator
  whiteSpace
  s2 <- member
  whiteSpace
  return $ ComparisonMember s1 cmp s2

cycle' :: Parser Cycle
cycle' = mapCycle

mapCycle :: Parser Cycle
mapCycle = do
  whiteSpace
  reserved "map"
  whiteSpace
  _ <- string "("
  whiteSpace
  i <- identifier
  reservedOp ","
  l <- mapList
  whiteSpace
  _ <- string ")"
  whiteSpace
  return $
    Cycle
      { mapF = i,
        mapL = l
      }

mapList :: Parser CycleList
mapList =
  try (CycleList <$> lists)
    <|> try (CycleId <$> identifier)

funcPattern :: Parser FuncBody
funcPattern = do
  whiteSpace
  reserved "pattern"
  whiteSpace
  reservedOp "{"
  whiteSpace
  c <- casePatterns
  whiteSpace
  d <- defaultPattern
  whiteSpace
  reservedOp "}"
  whiteSpace
  return $ FuncPattern c d

casePatterns :: Parser [PatternCase]
casePatterns = many casePattern

casePattern :: Parser PatternCase
casePattern = do
  whiteSpace
  reserved "case"
  whiteSpace
  _ <- string "("
  whiteSpace
  c <- casePatternVals
  whiteSpace
  _ <- string ")"
  whiteSpace
  _ <- string "{"
  whiteSpace
  s <- statement
  whiteSpace
  _ <- string "}"
  whiteSpace
  return $ PatternCase c s

defaultPattern :: Parser PatternDefault
defaultPattern = do
  whiteSpace
  reserved "default"
  whiteSpace
  reservedOp "{"
  whiteSpace
  s <- statement
  whiteSpace
  reservedOp "}"
  whiteSpace
  return $ PatternDefault s

casePatternVal :: Parser PatternCaseValue
casePatternVal =
  try casePatternEmpty
    <|> try casePatternValue

casePatternEmpty :: Parser PatternCaseValue
casePatternEmpty = do
  whiteSpace
  _ <- string "_"
  whiteSpace
  return PatternCaseEmpty

casePatternValue :: Parser PatternCaseValue
casePatternValue = do
  whiteSpace
  v <- value'
  whiteSpace
  return $ PatternCaseValue v

casePatternVals :: Parser [PatternCaseValue]
casePatternVals = sepBy casePatternVal (char ',')

funcCall :: Parser FuncCall
funcCall = do
  whiteSpace
  i <- identifier
  whiteSpace
  _ <- string "("
  whiteSpace
  p <- funcCallParams
  whiteSpace
  _ <- string ")"
  return $ FuncCall i p

funcCallParams :: Parser [FuncCallParam]
funcCallParams = sepBy funcCallParam (char ',')

funcCallParam :: Parser FuncCallParam
funcCallParam =
  try funcCallParamVal
    <|> try funcCallParamFC
    <|> try funcCallId

funcCallParamVal :: Parser FuncCallParam
funcCallParamVal = do
  whiteSpace
  v <- value'
  whiteSpace
  return $ FuncCallParamValue v

funcCallParamFC :: Parser FuncCallParam
funcCallParamFC = do
  whiteSpace
  v <- funcCall
  whiteSpace
  return $ FuncCallParam v

funcCallId :: Parser FuncCallParam
funcCallId = do
  whiteSpace
  v <- identifier
  whiteSpace
  return $ FuncCallIdentifier v

doNotation :: Parser DoNotation
doNotation = do
  reserved "do"
  whiteSpace
  reservedOp "{"
  whiteSpace
  c <- doStatements
  whiteSpace
  reservedOp "}"
  whiteSpace
  return $ DoNotation c

doStatements :: Parser [DoStatement]
doStatements = many doStatement

doStatement :: Parser DoStatement
doStatement = try doAssignment <|> try doPrint

doAssignment :: Parser DoStatement
doAssignment = do
  whiteSpace
  reserved "let"
  whiteSpace
  i <- identifier
  _ <- string ":"
  t <- dataType
  whiteSpace
  _ <- string "="
  whiteSpace
  s <- statement
  whiteSpace
  return $ DoAssignment i (str2type t) s

doPrint :: Parser DoStatement
doPrint = do
  whiteSpace
  reserved "print"
  whiteSpace
  _ <- string "("
  whiteSpace
  s <- try printStatement <|> try printRef
  whiteSpace
  _ <- string ")"
  whiteSpace
  return $ DoPrint s

printStatement :: Parser Print
printStatement = PrintStatement <$> statement

printRef :: Parser Print
printRef = PrintRef <$> identifier