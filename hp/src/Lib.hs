module Lib (funcParam, funcParams) where

import AbstractSyntaxTree
import Lexer
import Text.Parsec
import Text.Parsec.String (Parser)

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
  _ <- many $ char '\n' <|> char '\0'
  reserved "params"
  spaces
  reservedOp "{"
  _ <- many $ char '\n' <|> char '\0'
  params <- funcParams
  _ <- many $ char '\n' <|> char '\0'
  reservedOp "}"
  _ <- many $ char '\n' <|> char '\0'
  body <- funcBody
  _ <- many $ char '\n' <|> char '\0'
  reservedOp "}"
  _ <- many $ char '\n' <|> char '\0'
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

funcBody = funcReturn

funcReturn = do
  _ <- many $ char '\n' <|> char '\0'
  reserved "return"
  _ <- many $ char '\n' <|> char '\0'
  reservedOp "{"
  _ <- many $ char '\n' <|> char '\0'
  s <- statement
  _ <- many $ char '\n' <|> char '\0'
  reservedOp "}"
  return $ FuncReturn s

statement =
  SValue <$> value'

value' =
  ValTask <$> task'
    <|> ValMember <$> member
    <|> ValList <$> lists
    <|> ValTag <$> tags

task' = do
  _ <- many $ char '\n' <|> char '\0'
  _ <- string "Task {"
  _ <- many $ char '\n' <|> char '\0'
  _ <- string "Task {"

  _ <- many $ char '\n' <|> char '\0'
  _ <- string "title: "
  _ <- many $ char '\n' <|> char '\0'
  t <- taskTitle
  _ <- many $ char '\n' <|> char '\0'
  reservedOp ","
  _ <- many $ char '\n' <|> char '\0'
  _ <- string "description: "
  _ <- many $ char '\n' <|> char '\0'
  d <- taskDescription
  _ <- many $ char '\n' <|> char '\0'
  reservedOp ","

  _ <- many $ char '\n' <|> char '\0'
  _ <- string "state: "
  _ <- many $ char '\n' <|> char '\0'
  s <- taskState
  _ <- many $ char '\n' <|> char '\0'
  reservedOp ","

  _ <- many $ char '\n' <|> char '\0'
  _ <- string "members: "
  _ <- many $ char '\n' <|> char '\0'
  m <- taskMembers
  _ <- many $ char '\n' <|> char '\0'
  reservedOp ","

  _ <- many $ char '\n' <|> char '\0'
  _ <- string "tag: "
  _ <- many $ char '\n' <|> char '\0'
  tg <- taskTag
  _ <- many $ char '\n' <|> char '\0'
  reservedOp ","

  _ <- many $ char '\n' <|> char '\0'
  _ <- string "tag: "
  _ <- many $ char '\n' <|> char '\0'
  st <- taskSubTasks

  _ <- many $ char '\n' <|> char '\0'

  reservedOp "}"
  _ <- many $ char '\n' <|> char '\0'
  return $
    Task
      { title = t,
        description = d,
        state = s,
        members = m,
        tag = tg,
        subTasks = st
      }

taskTitle =
  TVTitle <$> identifierWithSpace
    <|> TITitle <$> identifier

taskDescription =
  TVDescription <$> identifierWithSpace
    <|> TIDescription <$> identifier

taskState =
  TVState <$> identifierWithSpace
    <|> TIState <$> identifier

taskMembers =
  try
    ( TMembersId
        <$> many (char '\n' <|> char '\0')
        <* identifier
        <* many (char '\n' <|> char '\0')
    )
    <|> TMembersValue <$> listOfMembers

taskSubTasks =
  try
    ( TSubTasksId
        <$> many (char '\n' <|> char '\0')
        <* identifier
        <* many (char '\n' <|> char '\0')
    )
    <|> TSubTasksValue <$> lists

lists = listOfMembers <|> listOfTasks

listOfMembers = do
  _ <- many $ char '\n' <|> char '\0'
  _ <- string "List:Member ["
  _ <- many $ char '\n' <|> char '\0'
  m <- sepBy member (char ',')
  _ <- many $ char '\n' <|> char '\0'
  _ <- string "]"
  _ <- many $ char '\n' <|> char '\0'
  return $ ListMember m

listOfTasks = do
  _ <- many $ char '\n' <|> char '\0'
  _ <- string "List:Task ["
  _ <- many $ char '\n' <|> char '\0'
  m <- sepBy task' (char ',')
  _ <- many $ char '\n' <|> char '\0'
  _ <- string "]"
  _ <- many $ char '\n' <|> char '\0'
  return $ ListTask m

tags =
  try
    ( Tag <$> identifierWithSpace
    )
    <|> do
      _ <- many $ char '\n' <|> char '\0'
      _ <- string "NoTag"
      _ <- many $ char '\n' <|> char '\0'
      return NoTag

taskTag = TVTag <$> tags <|> TITag <$> identifier

member =
  try
    ( NoAssigned
        <$ many (char '\n' <|> char '\0')
        <* string "NoAssigned"
        <* many (char '\n' <|> char '\0')
    )
    <|> do
      _ <- many $ char '\n' <|> char '\0'
      _ <- string "Member {"
      _ <- many $ char '\n' <|> char '\0'
      _ <- string "name: "
      _ <- many $ char '\n' <|> char '\0'
      n <- memberName
      _ <- many $ char '\n' <|> char '\0'
      reservedOp ","

      _ <- many $ char '\n' <|> char '\0'
      _ <- string "role: "
      _ <- many $ char '\n' <|> char '\0'
      r <- memberRole
      _ <- many $ char '\n' <|> char '\0'
      _ <- string "}"
      _ <- many $ char '\n' <|> char '\0'
      return $ Member n r

memberName =
  MVName <$> identifierWithSpace
    <|> MIName <$> identifier

memberRole =
  MVRole <$> identifierWithSpace
    <|> MIRole <$> identifier

identifierWithSpace :: Parser String
identifierWithSpace = do
  _ <- many $ char '\n' <|> char '\0'
  _ <- char '\"'
  v <- letter
  r <- many (letter <|> digit <|> space)
  _ <- char '\"'
  _ <- many $ char '\n' <|> char '\0'
  return $ v : r

str2type :: String -> Type
str2type str = read $ 'T' : strType
  where
    strType = filter (/= ':') str
