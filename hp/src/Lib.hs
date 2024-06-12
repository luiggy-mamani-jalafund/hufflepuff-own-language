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
    takeTaskAttribute,
  )
where

import AbstractSyntaxTree
import Lexer
import Text.Parsec
import Text.Parsec.String (Parser)

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
lists =
  try listOfTasks
    <|> try listOfMembers
    <|> try listOfBool

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

listOfBool :: Parser List
listOfBool = do
  whiteSpace
  _ <- string "List:Task"
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
  return $ TMAName i

tmaRole :: Parser TakeMemberAttribute
tmaRole = do
  whiteSpace
  i <- identifier
  _ <- string ".role"
  whiteSpace
  return $ TMARole i

takeTaskAttribute :: Parser TakeTaskAttribute
takeTaskAttribute =
  try (TTAStrings <$> ttaStrings)
    <|> try ttaMembers
    <|> try ttaSubTasks

ttaStrings :: Parser TTAStrings
ttaStrings =
  try ttaTitle
    <|> try ttaDescription
    <|> try ttaState
    <|> try ttaTag

ttaTitle :: Parser TTAStrings
ttaTitle = do
  whiteSpace
  i <- identifier
  _ <- string ".title"
  whiteSpace
  return $ TTATitle i

ttaDescription :: Parser TTAStrings
ttaDescription = do
  whiteSpace
  i <- identifier
  _ <- string ".description"
  whiteSpace
  return $ TTADescription i

ttaState :: Parser TTAStrings
ttaState = do
  whiteSpace
  i <- identifier
  _ <- string ".state"
  whiteSpace
  return $ TTAState i

ttaTag :: Parser TTAStrings
ttaTag = do
  whiteSpace
  i <- identifier
  _ <- string ".tag"
  whiteSpace
  return $ TTATag i

ttaMembers :: Parser TakeTaskAttribute
ttaMembers = do
  whiteSpace
  i <- identifier
  _ <- string ".members"
  whiteSpace
  return $ TTAMembers i

ttaSubTasks :: Parser TakeTaskAttribute
ttaSubTasks = do
  whiteSpace
  i <- identifier
  _ <- string ".subTasks"
  whiteSpace
  return $ TTASubTasks i

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
    <|> try (LTTAStrings <$> ttaStrings)
    <|> try (LTMAStrings <$> takeMemberAttribute)

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
