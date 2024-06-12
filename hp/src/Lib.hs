module Lib
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
  )
where

import AbstractSyntaxTree
import Lexer
import Text.Parsec
import Text.Parsec.String (Parser)

func :: Parser Func
func = do
  reserved "func"
  whiteSpace
  funId <- identifier
  whiteSpace
  reservedOp "->"
  whiteSpace
  funType <- dataType
  whiteSpace
  reservedOp "{"
  whiteSpace
  reserved "params"
  whiteSpace
  reservedOp "{"
  whiteSpace
  params <- funcParams
  whiteSpace
  reservedOp "}"
  whiteSpace
  body <- funcBody
  whiteSpace
  reservedOp "}"
  whiteSpace
  return $ Func funId (str2type funType) params body

funcParam :: Parser FunParam
funcParam = do
  whiteSpace
  i <- identifier
  whiteSpace
  reservedOp ":"
  whiteSpace
  paramType <- dataType
  whiteSpace
  return $ FunParam i (str2type paramType)

funcParams :: Parser [FunParam]
funcParams = sepBy funcParam (char ',')

funcBody :: Parser FuncBody
funcBody = funcReturn

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
  SBoolCondition <$> condition
    <|> SBoolExp <$> boolExp
    <|> SValue <$> value'

value' :: Parser Value
value' =
  ValLiteral <$> literal
    <|> ValTask <$> task'
    <|> ValMember <$> member
    <|> ValList <$> lists
    <|> ValTag <$> tag'

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
  reservedOp "}"
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
  TVTitle . StrIdSpaces <$> strIdSpaces
    <|> TITitle <$> identifier

taskDescription :: Parser DescriptionTask
taskDescription =
  TVDescription . StrParagraph <$> strParagraph
    <|> TIDescription <$> identifier

taskState :: Parser StateTask
taskState =
  TVState . StrId <$> strId
    <|> TIState <$> identifier

taskMembers :: Parser MembersTask
taskMembers =
  try $
    TMembersValue <$> listOfMembers
      <|> TMembersId
        <$ whiteSpace
        <*> identifier
        <* whiteSpace

taskSubTasks :: Parser SubTasksTask
taskSubTasks =
  try $
    TSubTasksValue <$> lists
      <|> ( TSubTasksId
              <$ whiteSpace
              <*> identifier
              <* whiteSpace
          )

lists :: Parser List
lists = try listOfTasks <|> try listOfMembers

listOfMembers :: Parser List
listOfMembers = do
  whiteSpace
  _ <- string "List:Member"
  whiteSpace
  _ <- string "["
  whiteSpace
  m <- sepBy member (char ',')
  whiteSpace
  _ <- string "]"
  whiteSpace
  return $ ListMember m

listOfTasks :: Parser List
listOfTasks = do
  whiteSpace
  _ <- string "List:Task"
  whiteSpace
  _ <- string "["
  whiteSpace
  m <- sepBy task' (char ',')
  whiteSpace
  _ <- string "]"
  whiteSpace
  return $ ListTask m

tag' :: Parser Tag
tag' =
  try $
    Tag . StrId <$> strId
      <|> do
        whiteSpace
        _ <- string "NoTag"
        whiteSpace
        return NoTag

taskTag :: Parser TagTask
taskTag = TVTag <$> tag' <|> TITag <$> identifier

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
  MVName . StrIdSpaces <$> strIdSpaces
    <|> MIName <$> identifier

memberRole :: Parser MemberRole
memberRole =
  MVRole . StrIdSpaces <$> strIdSpaces
    <|> MIRole <$> identifier

literal :: Parser Literal
literal =
  try (LStringId . StrId <$> strId)
    <|> try (LStringIdSpaces . StrIdSpaces <$> strIdSpaces)
    <|> try (LStringParagraph . StrParagraph <$> strParagraph)

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
  r <- many (alphaNum <|> space)
  _ <- char '\"'
  whiteSpace
  return $ v : r

strParagraph :: Parser String
strParagraph = do
  whiteSpace
  _ <- char '\"'
  v <- many (alphaNum <|> space <|> oneOf ".,;?¿!¡")
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
  reservedOp "("
  whiteSpace
  e <- boolExp
  whiteSpace
  reservedOp ")"
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
  BComparison <$ whiteSpace <*> comparison <* whiteSpace
    <|> BExp <$ whiteSpace <*> boolVal <* whiteSpace

comparison :: Parser Comparison
comparison = try boolComparison <|> try strComparison

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
  return $ CString s1 cmp s2

boolComparison :: Parser Comparison
boolComparison = do
  s1 <- boolVal
  whiteSpace
  cmp <- boolComparator
  whiteSpace
  s2 <- boolVal
  whiteSpace
  return $ CBool s1 cmp s2
