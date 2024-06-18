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
    <|> try (SBoolExp <$> boolExp)
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
    <|> try (ValBool <$> boolVal)

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
  try (TaskTakeTitle <$> ttaTitle)
    <|> try (TaskValueTitle . StrIdSpaces <$> strIdSpaces)
    <|> try (TaskIdentifierTitle <$> identifier)

taskDescription :: Parser DescriptionTask
taskDescription =
  try (TaskTakeDescription <$> ttaDescription)
    <|> try (TaskValueDescription . StrParagraph <$> strParagraph)
    <|> try (TaskIdentifierDescription <$> identifier)

taskState :: Parser StateTask
taskState =
  try (TaskTakeState <$> ttaState)
    <|> try (TaskValueState <$> state')
    <|> try (TaskIdentifierState <$> identifier)

taskMembers :: Parser MembersTask
taskMembers =
  try (TaskTakeMembers <$> ttaMembers)
    <|> try (TaskValueMembers <$> listOfMembers)
    <|> try
      ( TaskIdentifierMembers
          <$ whiteSpace
          <*> identifier
          <* whiteSpace
      )

taskTag :: Parser TagTask
taskTag =
  try (TaskTakeTag <$> ttaTag)
    <|> try (TaskValueTag <$> tag')
    <|> try (TaskIdentifierTag <$> identifier)

taskSubTasks :: Parser SubTasksTask
taskSubTasks =
  try (TaskTakeSubTasks <$> ttaSubTasks)
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
    <|> try listOfStrIdSpaces
    <|> try listOfStrParagraph
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
  i <- sepBy (StrId <$> strId) (char ',')
  whiteSpace
  _ <- string "]"
  whiteSpace
  return $ ListStringId i

listOfStrIdSpaces :: Parser List
listOfStrIdSpaces = do
  whiteSpace
  _ <- string "List:StringIdSpace"
  whiteSpace
  _ <- string "["
  whiteSpace
  i <- sepBy (StrIdSpaces <$> strIdSpaces) (char ',')
  whiteSpace
  _ <- string "]"
  whiteSpace
  return $ ListStringIdSpace i

listOfStrParagraph :: Parser List
listOfStrParagraph = do
  whiteSpace
  _ <- string "List:StringParagraph"
  whiteSpace
  _ <- string "["
  whiteSpace
  i <- sepBy (StrParagraph <$> strParagraph) (char ',')
  whiteSpace
  _ <- string "]"
  whiteSpace
  return $ ListStringParagraph i

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
  i <- sepBy boolVal (char ',')
  whiteSpace
  _ <- string "]"
  whiteSpace
  return $ ListBool i

tag' :: Parser Tag
tag' =
  try $
    Tag . StrId <$> strId
      <|> do
        whiteSpace
        _ <- string "NoTag"
        whiteSpace
        return NoTag

state' :: Parser TaskState
state' = StrId <$> strId

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
  try (TakeTaskAttributeStrings <$> ttaStrings)
    <|> try (TakeTaskAttributeMembers <$> ttaMembers)
    <|> try (TakeTaskAttributeSubTasks <$> ttaSubTasks)

ttaStrings :: Parser TakeTaskAttributeLiteral
ttaStrings =
  try (TakeTaskAttributeState <$> ttaState)
    <|> try (TakeTaskAttributeTitle <$> ttaTitle)
    <|> try (TakeTaskAttributeDescription <$> ttaDescription)
    <|> try (TakeTaskAttributeTag <$> ttaTag)

ttaTitle :: Parser String
ttaTitle = do
  whiteSpace
  i <- identifier
  _ <- string ".title"
  whiteSpace
  return i

ttaDescription :: Parser String
ttaDescription = do
  whiteSpace
  i <- identifier
  _ <- string ".description"
  whiteSpace
  return i

ttaState :: Parser String
ttaState = do
  whiteSpace
  i <- identifier
  _ <- string ".state"
  whiteSpace
  return i

ttaTag :: Parser String
ttaTag = do
  whiteSpace
  i <- identifier
  _ <- string ".tag"
  whiteSpace
  return i

ttaMembers :: Parser String
ttaMembers = do
  whiteSpace
  i <- identifier
  _ <- string ".members"
  whiteSpace
  return i

ttaSubTasks :: Parser String
ttaSubTasks = do
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
  MemberValueName . StrIdSpaces <$> strIdSpaces
    <|> MemberIdentifierName <$> identifier

memberRole :: Parser MemberRole
memberRole =
  MemberValueRole . StrIdSpaces <$> strIdSpaces
    <|> MemberIdentifierRole <$> identifier

literal :: Parser Literal
literal =
  try (LStringId . StrId <$> strId)
    <|> try (LStringIdSpaces . StrIdSpaces <$> strIdSpaces)
    <|> try (LStringParagraph . StrParagraph <$> strParagraph)
    <|> try (LTakeTaskAttribute <$> ttaStrings)
    <|> try (LTakeMemberAttribute <$> takeMemberAttribute)

strId :: Parser String
strId = do
  whiteSpace
  _ <- char '\"'
  v <- identifier
  _ <- char '\"'
  whiteSpace
  return v

strIdSpaces :: Parser String
strIdSpaces = do
  whiteSpace
  _ <- char '\"'
  v <- letter
  r <- many (alphaNum <|> space <|> char ':')
  _ <- char '\"'
  whiteSpace
  return $ v : r

strParagraph :: Parser String
strParagraph = do
  whiteSpace
  _ <- char '\"'
  v <- many (alphaNum <|> space <|> oneOf ".,;?¿!¡-:'")
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
  e <- boolExp
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

boolExp :: Parser BoolExpression
boolExp =
  BoolComparison <$ whiteSpace <*> comparison <* whiteSpace
    <|> BoolValue <$ whiteSpace <*> boolVal <* whiteSpace

comparison :: Parser Comparison
comparison =
  try boolComparison
    <|> try strComparison
    <|> try taskComparison
    <|> try memberComparison

boolVal :: Parser Bool
boolVal = try boolTrue <|> try boolFalse

boolTrue :: Parser Bool
boolTrue = do
  _ <- string "True"
  return True

boolFalse :: Parser Bool
boolFalse = do
  _ <- string "False"
  return False

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
  s1 <- boolVal
  whiteSpace
  cmp <- boolComparator
  whiteSpace
  s2 <- boolVal
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