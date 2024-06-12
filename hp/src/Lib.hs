module Lib
  ( func,
    funcParam,
    funcParams,
    funcBody,
    task',
    taskMembers,
    member,
  )
where

import AbstractSyntaxTree
import Lexer
import Text.Parsec
import Text.Parsec.String (Parser)

func :: Parser Func
func = do
  reserved "func"
  spaces
  funId <- identifier
  spaces
  reservedOp "->"
  spaces
  funType <- dataType
  let t = str2type funType
  spaces
  reservedOp "{"
  whiteSpace
  reserved "params"
  spaces
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
  return $ Func funId t params body

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
  SValue <$> value'

value' :: Parser Value
value' =
  ValTask <$> task'
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
  TVTitle <$> identifierWithSpace
    <|> TITitle <$> identifier

taskDescription :: Parser DescriptionTask
taskDescription =
  TVDescription <$> identifierWithSpace
    <|> TIDescription <$> identifier

taskState :: Parser StateTask
taskState =
  TVState <$> identifierWithSpace
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
    Tag <$> identifierWithSpace
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
    return $ Member n r
    <|> NoAssigned
      <$ whiteSpace
      <* string "NoAssigned"
      <* whiteSpace

memberName :: Parser MemberName
memberName =
  MVName <$> identifierWithSpace
    <|> MIName <$> identifier

memberRole  :: Parser MemberRole
memberRole =
  MVRole <$> identifierWithSpace
    <|> MIRole <$> identifier

identifierWithSpace :: Parser String
identifierWithSpace = do
  whiteSpace
  _ <- char '\"'
  v <- letter
  r <- many (letter <|> digit <|> space)
  _ <- char '\"'
  whiteSpace
  return $ v : r

str2type :: String -> Type
str2type str = read $ 'T' : strType
  where
    strType = filter (/= ':') str
