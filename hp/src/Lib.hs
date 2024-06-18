{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
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
    casePattern,
    casePatternVal,
    casePatternVals,
    code,
    parseCode
  )
where

import AbstractSyntaxTree
import LexerP
import Text.Parsec
import Text.Parsec.String  (Parser)
import SymbolTable

parseCode :: Parser (Code, SymbolTable)
parseCode = do
  let initialSymTable = emptyTable
  code initialSymTable


code :: SymbolTable -> Parser (Code, SymbolTable)
code symTable = do
  whiteSpace
  (f, symTable1) <- funcs symTable
  whiteSpace
  (d, symTable2) <- doStatement symTable1
  whiteSpace
  return (Code f d, symTable2)

doStatement :: SymbolTable -> Parser (DoStatement, SymbolTable)
doStatement symTable = do
  reserved "do"
  whiteSpace
  reservedOp "{"
  whiteSpace
  (c, symTable1) <- funcCall symTable
  whiteSpace
  reservedOp "}"
  whiteSpace
  return (DoStatement c, symTable1)


funcs :: (Read Type) => SymbolTable -> Parser ([Func], SymbolTable)
funcs symTable = manyAccum func symTable
  where
    manyAccum p s = do
      res <- optionMaybe (p s)
      case res of
        Nothing -> return ([], s)
        Just (x, s1) -> do
          (xs, s2) <- manyAccum p s1
          return (x:xs, s2)


func :: (Read Type) => SymbolTable -> Parser (Func, SymbolTable)
func symTable = do
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
  (params, symTable1) <- funcParams symTable
  whiteSpace
  _ <- string  "}"
  whiteSpace
  (body, symTable2) <- funcBody symTable1
  whiteSpace
  _ <- string  "}"
  whiteSpace
  let newSymTable = insertFunction funId (str2type funType) params body symTable2
  return (Func funId (str2type funType) params body, newSymTable)


funcParam :: (Read Type) => SymbolTable -> Parser (FunParam, SymbolTable)
funcParam symTable = do
  whiteSpace
  i <- identifier
  whiteSpace
  reservedOp ":"
  whiteSpace
  paramType <- dataType
  whiteSpace
  return (FunParam i (str2type paramType), symTable)

funcParams :: (Read Type) => SymbolTable -> Parser ([FunParam], SymbolTable)
funcParams symTable = sepByAccum funcParam symTable
  where
    sepByAccum p s = do
      res <- optionMaybe (p s)
      case res of
        Nothing -> return ([], s)
        Just (x, s1) -> do
          _ <- optionMaybe (char ',')
          (xs, s2) <- sepByAccum p s1
          return (x:xs, s2)


funcBody :: SymbolTable -> Parser (FuncBody, SymbolTable)
funcBody symTable = try (funcReturn symTable) <|> try (funcPattern symTable)


funcReturn :: SymbolTable -> Parser (FuncBody, SymbolTable)
funcReturn symTable = do
  whiteSpace
  reserved "return"
  whiteSpace
  reservedOp "{"
  whiteSpace
  (s, symTable1) <- statement symTable
  whiteSpace
  reservedOp "}"
  return (FuncReturn s, symTable1)


statement :: SymbolTable -> Parser (Statement, SymbolTable)
statement symTable = 
  try (do
    (cond, symTable1) <- condition symTable
    return (SBoolCondition cond, symTable1))
  <|> try (do
    (boolExp, symTable1) <- boolExp symTable
    return (SBoolExp boolExp, symTable1))
  <|> try (do
    (val, symTable1) <- value' symTable
    return (SValue val, symTable1))
  <|> try (do
    (cycle, symTable1) <- cycle' symTable
    return (SCycle cycle, symTable1))
  <|> try (do
    (funcCall, symTable1) <- funcCall symTable
    return (SFuncCall funcCall, symTable1))
  <|> try (do
    (taskAttr, symTable1) <- takeTaskAttribute symTable
    return (STTA taskAttr, symTable1))
  <|> try (do
    (memberAttr, symTable1) <- takeMemberAttribute symTable
    return (STMA memberAttr, symTable1))



value' :: SymbolTable -> Parser (Value, SymbolTable)
value' symTable = 
  try (do
    (val, symTable1) <- literal symTable
    return (ValLiteral val, symTable1))
  <|> try (do
    (task, symTable2) <- task' symTable
    return (ValTask task, symTable2))
  <|> try (do
    (member, symTable3) <- member symTable
    return (ValMember member, symTable3))
  <|> try (do
    (list, symTable4) <- lists symTable
    return (ValList list, symTable4))
  <|> try (do
    (tag, symTable5) <- tag' symTable
    return (ValTag tag, symTable5))
  <|> try (do
    (boolVal, symTable6) <- boolVal symTable
    return (ValBool boolVal, symTable6))


task' :: SymbolTable -> Parser (Task, SymbolTable)
task' symTable = do
  whiteSpace
  _ <- string "Task"
  whiteSpace
  _ <- string "{"

  whiteSpace
  _ <- string "title:"
  whiteSpace
  (t, symTable1) <- taskTitle symTable
  whiteSpace
  reservedOp ","

  whiteSpace
  _ <- string "description:"
  whiteSpace
  (d, symTable2) <- taskDescription symTable1
  whiteSpace
  reservedOp ","

  whiteSpace
  _ <- string "state:"
  whiteSpace
  (s, symTable3) <- taskState symTable2
  whiteSpace
  reservedOp ","

  whiteSpace
  _ <- string "members:"
  whiteSpace
  (m, symTable4) <- taskMembers symTable3
  whiteSpace
  reservedOp ","

  whiteSpace
  _ <- string "tag:"
  whiteSpace
  (tg, symTable5) <- taskTag symTable4
  whiteSpace
  reservedOp ","

  whiteSpace
  _ <- string "subTasks:"
  whiteSpace
  (st, symTable6) <- taskSubTasks symTable5

  whiteSpace
  _ <- string "}"
  whiteSpace
  return (
    Task
      { title = t,
        description = d,
        status = s,
        members = m,
        tag = tg,
        subTasks = st
      }, symTable6)


taskTitle :: SymbolTable -> Parser (TitleTask, SymbolTable)
taskTitle symTable = 
  try (do
    (ttaTitle, symTable1) <- ttaTitle symTable
    return (TTTitle ttaTitle, symTable1))
  <|> try (do
    strIdSpaces <- strIdSpaces symTable
    return (TVTitle (StrIdSpaces (fst strIdSpaces)), snd strIdSpaces))
  <|> try (do
    ident <- identifier
    return (TITitle ident, symTable))

    
taskDescription :: SymbolTable -> Parser (DescriptionTask, SymbolTable)
taskDescription symTable = 
  try (do
    (ttaDescription, symTable1) <- ttaDescription symTable
    return (TTDescription ttaDescription, symTable1))
  <|> try (do
    strParagraph <- strParagraph symTable
    return (TVDescription (StrParagraph (fst strParagraph)), snd strParagraph))
  <|> try (do
    ident <- identifier
    return (TIDescription ident, symTable))


taskState :: SymbolTable -> Parser (StateTask, SymbolTable)
taskState symTable = 
  try (do
    (ttaState, symTable1) <- ttaState symTable
    return (TTState ttaState, symTable1))
  <|> try (do
    state <- state' symTable
    return (TVState (fst state), snd state))
  <|> try (do
    ident <- identifier
    return (TIState ident, symTable))


taskMembers :: SymbolTable -> Parser (MembersTask, SymbolTable)
taskMembers symTable = 
  try (do
    (ttaMembers, symTable1) <- ttaMembers symTable
    return (TTMembers ttaMembers, symTable1))
  <|> try (do
    (listOfMembers, symTable2) <- listOfMembers symTable
    return (TVMembers listOfMembers, symTable2))
  <|> try (do
    whiteSpace
    ident <- identifier
    whiteSpace
    return (TIMembers ident, symTable))


taskTag :: SymbolTable -> Parser (TagTask, SymbolTable)
taskTag symTable = 
  try (do
    (ttaTag, symTable1) <- ttaTag symTable
    return (TTTag ttaTag, symTable1))
  <|> try (do
    (tag, symTable2) <- tag' symTable
    return (TVTag tag, symTable2))
  <|> try (do
    ident <- identifier
    return (TITag ident, symTable))



taskSubTasks :: SymbolTable -> Parser (SubTasksTask, SymbolTable)
taskSubTasks symTable = 
  try (do
    (ttaSubTasks, symTable1) <- ttaSubTasks symTable
    return (TTSubTasks ttaSubTasks, symTable1))
  <|> try (do
    (lists, symTable2) <- lists symTable
    return (TVSubTasks lists, symTable2))
  <|> try (do
    whiteSpace
    ident <- identifier
    whiteSpace
    return (TISubTasks ident, symTable))

      
lists :: SymbolTable -> Parser (List, SymbolTable)
lists symTable =
  try (listOfTasks symTable)
    <|> try (listOfMembers symTable)
    <|> try (listOfStates symTable)
    <|> try (listOfTags symTable)
    <|> try (listOfStrId symTable)
    <|> try (listOfStrIdSpaces symTable)
    <|> try (listOfStrParagraph symTable)
    <|> try (listOfBool symTable)
    <|> try (listOfLists symTable)


listOfLists :: SymbolTable -> Parser (List, SymbolTable)
listOfLists symTable = do
  whiteSpace
  _ <- string "List:List"
  whiteSpace
  _ <- string "["
  whiteSpace
  (i, symTable1) <- sepByAccum lists symTable
  whiteSpace
  _ <- string "]"
  whiteSpace
  return (ListList i, symTable1)
  where
    sepByAccum p s = do
      res <- optionMaybe (p s)
      case res of
        Nothing -> return ([], s)
        Just (x, s1) -> do
          _ <- optionMaybe (char ',')
          (xs, s2) <- sepByAccum p s1
          return (x:xs, s2)

listOfStrId :: SymbolTable -> Parser (List, SymbolTable)
listOfStrId symTable = do
  whiteSpace
  _ <- string "List:StringId"
  whiteSpace
  _ <- string "["
  whiteSpace
  (i, symTable1) <- sepByAccum strId symTable
  whiteSpace
  _ <- string "]"
  whiteSpace
  return (ListStringId (map StrId i), symTable1)
  where
    sepByAccum :: (SymbolTable -> Parser (a, SymbolTable)) -> SymbolTable -> Parser ([a], SymbolTable)
    sepByAccum p s = do
      res <- optionMaybe (p s)
      case res of
        Nothing -> return ([], s)
        Just ((x, s1)) -> do
          _ <- optionMaybe (char ',')
          (xs, s2) <- sepByAccum p s1
          return (x:xs, s2)


listOfStrIdSpaces :: SymbolTable -> Parser (List, SymbolTable)
listOfStrIdSpaces symTable = do
  whiteSpace
  _ <- string "List:StringIdSpace"
  whiteSpace
  _ <- string "["
  whiteSpace
  (i, symTable1) <- sepByAccum strIdSpaces symTable
  whiteSpace
  _ <- string "]"
  whiteSpace
  return (ListStringIdSpace (map StrIdSpaces i), symTable1)
  where
    sepByAccum :: (SymbolTable -> Parser (a, SymbolTable)) -> SymbolTable -> Parser ([a], SymbolTable)
    sepByAccum p s = do
      res <- optionMaybe (p s)
      case res of
        Nothing -> return ([], s)
        Just (x, s1) -> do
          _ <- optionMaybe (char ',')
          (xs, s2) <- sepByAccum p s1
          return (x:xs, s2)


listOfStrParagraph :: SymbolTable -> Parser (List, SymbolTable)
listOfStrParagraph symTable = do
  whiteSpace
  _ <- string "List:StringParagraph"
  whiteSpace
  _ <- string "["
  whiteSpace
  (i, symTable1) <- sepByAccum strParagraph symTable
  whiteSpace
  _ <- string "]"
  whiteSpace
  return (ListStringParagraph (map StrParagraph i), symTable1)
  where
    sepByAccum :: (SymbolTable -> Parser (a, SymbolTable)) -> SymbolTable -> Parser ([a], SymbolTable)
    sepByAccum p s = do
      res <- optionMaybe (p s)
      case res of
        Nothing -> return ([], s)
        Just (x, s1) -> do
          _ <- optionMaybe (char ',')
          (xs, s2) <- sepByAccum p s1
          return (x:xs, s2)

listOfMembers :: SymbolTable -> Parser (List, SymbolTable)
listOfMembers symTable = do
  whiteSpace
  _ <- string "List:Member"
  whiteSpace
  _ <- string "["
  whiteSpace
  (i, symTable1) <- sepByAccum member symTable
  whiteSpace
  _ <- string "]"
  whiteSpace
  return (ListMember i, symTable1)
  where
    sepByAccum p s = do
      res <- optionMaybe (p s)
      case res of
        Nothing -> return ([], s)
        Just (x, s1) -> do
          _ <- optionMaybe (char ',')
          (xs, s2) <- sepByAccum p s1
          return (x:xs, s2)

listOfTasks :: SymbolTable -> Parser (List, SymbolTable)
listOfTasks symTable = do
  whiteSpace
  _ <- string "List:Task"
  whiteSpace
  _ <- string "["
  whiteSpace
  (i, symTable1) <- sepByAccum task' symTable
  whiteSpace
  _ <- string "]"
  whiteSpace
  return (ListTask i, symTable1)
  where
    sepByAccum p s = do
      res <- optionMaybe (p s)
      case res of
        Nothing -> return ([], s)
        Just (x, s1) -> do
          _ <- optionMaybe (char ',')
          (xs, s2) <- sepByAccum p s1
          return (x:xs, s2)

listOfTags :: SymbolTable -> Parser (List, SymbolTable)
listOfTags symTable = do
  whiteSpace
  _ <- string "List:Tag"
  whiteSpace
  _ <- string "["
  whiteSpace
  (i, symTable1) <- sepByAccum tag' symTable
  whiteSpace
  _ <- string "]"
  whiteSpace
  return (ListTag i, symTable1)
  where
    sepByAccum p s = do
      res <- optionMaybe (p s)
      case res of
        Nothing -> return ([], s)
        Just (x, s1) -> do
          _ <- optionMaybe (char ',')
          (xs, s2) <- sepByAccum p s1
          return (x:xs, s2)

listOfStates :: SymbolTable -> Parser (List, SymbolTable)
listOfStates symTable = do
  whiteSpace
  _ <- string "List:State"
  whiteSpace
  _ <- string "["
  whiteSpace
  (i, symTable1) <- sepByAccum state' symTable
  whiteSpace
  _ <- string "]"
  whiteSpace
  return (ListState i, symTable1)
  where
    sepByAccum p s = do
      res <- optionMaybe (p s)
      case res of
        Nothing -> return ([], s)
        Just (x, s1) -> do
          _ <- optionMaybe (char ',')
          (xs, s2) <- sepByAccum p s1
          return (x:xs, s2)

listOfBool :: SymbolTable -> Parser (List, SymbolTable)
listOfBool symTable = do
  whiteSpace
  _ <- string "List:Bool"
  whiteSpace
  _ <- string "["
  whiteSpace
  (i, symTable1) <- sepByAccum boolVal symTable
  whiteSpace
  _ <- string "]"
  whiteSpace
  return (ListBool i, symTable1)
  where
    sepByAccum :: (SymbolTable -> Parser (a, SymbolTable)) -> SymbolTable -> Parser ([a], SymbolTable)
    sepByAccum p s = do
      res <- optionMaybe (p s)
      case res of
        Nothing -> return ([], s)
        Just (x, s1) -> do
          _ <- optionMaybe (char ',')
          (xs, s2) <- sepByAccum p s1
          return (x:xs, s2)


tag' :: SymbolTable -> Parser (Tag, SymbolTable)
tag' symTable =
  try (do
    strId <- strId symTable
    return (Tag (StrId (fst strId)), snd strId))
  <|> do
    whiteSpace
    _ <- string "NoTag"
    whiteSpace
    return (NoTag, symTable)

state' :: SymbolTable -> Parser (TaskState, SymbolTable)
state' symTable = do
  strId <- strId symTable
  return (StrId (fst strId), snd strId)


member :: SymbolTable -> Parser (Member, SymbolTable)
member symTable = 
  try (do
    whiteSpace
    _ <- string "Member"
    whiteSpace
    _ <- string "{"
    whiteSpace

    _ <- string "name: "
    whiteSpace
    (n, symTable1) <- memberName symTable
    whiteSpace
    reservedOp ","
    whiteSpace

    _ <- string "role: "
    whiteSpace
    (r, symTable2) <- memberRole symTable1
    whiteSpace

    _ <- string "}"
    whiteSpace
    return (Member { name = n, role = r }, symTable2))
  <|> do
    whiteSpace
    _ <- string "NoAssigned"
    whiteSpace
    return (NoAssigned, symTable)


takeMemberAttribute :: SymbolTable -> Parser (TakeMemberAttribute, SymbolTable)
takeMemberAttribute symTable =
  try (tmaName symTable)
    <|> try (tmaRole symTable)

tmaName :: SymbolTable -> Parser (TakeMemberAttribute, SymbolTable)
tmaName symTable = do
  whiteSpace
  ident <- identifier
  _ <- string ".name"
  whiteSpace
  return (TMAName ident, symTable)


tmaRole :: SymbolTable -> Parser (TakeMemberAttribute, SymbolTable)
tmaRole symTable = do
  whiteSpace
  ident <- identifier
  _ <- string ".role"
  whiteSpace
  return (TMARole ident, symTable)


takeTaskAttribute :: SymbolTable -> Parser (TakeTaskAttribute, SymbolTable)
takeTaskAttribute symTable =
  try (do
    (ttaStrings, symTable1) <- ttaStrings symTable
    return (TTAStrings ttaStrings, symTable1))
  <|> try (do
    (ttaMembers, symTable2) <- ttaMembers symTable
    return (TTAMembers ttaMembers, symTable2))
  <|> try (do
    (ttaSubTasks, symTable3) <- ttaSubTasks symTable
    return (TTASubTasks ttaSubTasks, symTable3))

---

ttaStrings :: SymbolTable -> Parser (TTAStrings, SymbolTable)
ttaStrings symTable =
  try (do
    (state, symTable1) <- ttaState symTable
    return (TTAState state, symTable1))
  <|> try (do
    (title, symTable2) <- ttaTitle symTable
    return (TTATitle title, symTable2))
  <|> try (do
    (description, symTable3) <- ttaDescription symTable
    return (TTADescription description, symTable3))
  <|> try (do
    (tag, symTable4) <- ttaTag symTable
    return (TTATag tag, symTable4))


ttaTitle :: SymbolTable -> Parser (String, SymbolTable)
ttaTitle symTable = do
  whiteSpace
  i <- identifier
  _ <- string ".title"
  whiteSpace
  return (i, symTable)


ttaDescription :: SymbolTable -> Parser (String, SymbolTable)
ttaDescription symTable = do
  whiteSpace
  i <- identifier
  _ <- string ".description"
  whiteSpace
  return (i, symTable)


ttaState :: SymbolTable -> Parser (String, SymbolTable)
ttaState symTable = do
  whiteSpace
  i <- identifier
  _ <- string ".state"
  whiteSpace
  return (i, symTable)


ttaTag :: SymbolTable -> Parser (String, SymbolTable)
ttaTag symTable = do
  whiteSpace
  i <- identifier
  _ <- string ".tag"
  whiteSpace
  return (i, symTable)


ttaMembers :: SymbolTable -> Parser (String, SymbolTable)
ttaMembers symTable = do
  whiteSpace
  i <- identifier
  _ <- string ".members"
  whiteSpace
  return (i, symTable)


ttaSubTasks :: SymbolTable -> Parser (String, SymbolTable)
ttaSubTasks symTable = do
  whiteSpace
  i <- identifier
  _ <- string ".subTasks"
  whiteSpace
  return (i, symTable)


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

---

memberName :: SymbolTable -> Parser (MemberName, SymbolTable)
memberName symTable = 
  try (do
    strIdSpaces <- strIdSpaces symTable
    return (MVName (StrIdSpaces (fst strIdSpaces)), snd strIdSpaces))
  <|> try (do
    ident <- identifier
    return (MIName ident, symTable))

memberRole :: SymbolTable -> Parser (MemberRole, SymbolTable)
memberRole symTable = 
  try (do
    strIdSpaces <- strIdSpaces symTable
    return (MVRole (StrIdSpaces (fst strIdSpaces)), snd strIdSpaces))
  <|> try (do
    ident <- identifier
    return (MIRole ident, symTable))


literal :: SymbolTable -> Parser (Literal, SymbolTable)
literal symTable = 
  try (do
    strId <- strId symTable
    return (LStringId (StrId (fst strId)), snd strId))
  <|> try (do
    strIdSpaces <- strIdSpaces symTable
    return (LStringIdSpaces (StrIdSpaces (fst strIdSpaces)), snd strIdSpaces))
  <|> try (do
    strParagraph <- strParagraph symTable
    return (LStringParagraph (StrParagraph (fst strParagraph)), snd strParagraph))
  <|> try (do
    (ttaStrings, symTable1) <- ttaStrings symTable
    return (LTTAStrings ttaStrings, symTable1))
  <|> try (do
    (takeMemberAttribute, symTable2) <- takeMemberAttribute symTable
    return (LTMAStrings takeMemberAttribute, symTable2))


strId :: SymbolTable -> Parser (String, SymbolTable)
strId symTable = do
  whiteSpace
  _ <- char '\"'
  v <- identifier
  _ <- char '\"'
  whiteSpace
  return (v, symTable)


strIdSpaces :: SymbolTable -> Parser (String, SymbolTable)
strIdSpaces symTable = do
  whiteSpace
  _ <- char '\"'
  v <- letter
  r <- many (alphaNum <|> space <|> char ':')
  _ <- char '\"'
  whiteSpace
  return (v : r, symTable)


strParagraph :: SymbolTable -> Parser (String, SymbolTable)
strParagraph symTable = do
  whiteSpace
  _ <- char '\"'
  v <- many (alphaNum <|> space <|> oneOf ".,;?¿!¡-:'")
  _ <- char '\"'
  whiteSpace
  return (v, symTable)


str2type :: (Read Type) => String -> Type
str2type str = read $ 'T' : strType
  where
    strType = filter (/= ':') str

condition :: SymbolTable -> Parser (Condition, SymbolTable)
condition symTable = do
  reserved "if"
  whiteSpace
  reservedOp "("
  whiteSpace
  (e, symTable1) <- boolExp symTable
  whiteSpace
  reservedOp ")"
  whiteSpace

  reserved "then"
  whiteSpace
  (s1, symTable2) <- statement symTable1
  whiteSpace
  reserved "else"
  whiteSpace
  (s2, symTable3) <- statement symTable2
  whiteSpace
  return (Condition { ifCondition = e, thenStatement = s1, elseStatament = s2 }, symTable3)


boolExp :: SymbolTable -> Parser (BoolExpression, SymbolTable)
boolExp symTable =
  try (do
    (comparison, symTable1) <- comparison symTable
    return (BComparison comparison, symTable1))
  <|> try (do
    boolVal <- boolVal symTable
    return (BExp (fst boolVal), snd boolVal))


comparison :: SymbolTable -> Parser (Comparison, SymbolTable)
comparison symTable =
  try (do
    (comparison, symTable1) <- boolComparison symTable
    return (comparison, symTable1))
  <|> try (do
    (comparison, symTable2) <- strComparison symTable
    return (comparison, symTable2))
  <|> try (do
    (comparison, symTable3) <- taskComparison symTable
    return (comparison, symTable3))
  <|> try (do
    (comparison, symTable4) <- memberComparison symTable
    return (comparison, symTable4))


---

boolVal :: SymbolTable -> Parser (Bool, SymbolTable)
boolVal symTable = 
  try (boolTrue symTable)
  <|> try (boolFalse symTable)


boolTrue :: SymbolTable -> Parser (Bool, SymbolTable)
boolTrue symTable = do
  _ <- string "True"
  return (True, symTable)


boolFalse :: SymbolTable -> Parser (Bool, SymbolTable)
boolFalse symTable = do
  _ <- string "False"
  return (False, symTable)


strComparison :: SymbolTable -> Parser (Comparison, SymbolTable)
strComparison symTable = do
  (s1, symTable1) <- literal symTable
  whiteSpace
  cmp <- boolComparator
  whiteSpace
  (s2, symTable2) <- literal symTable1
  whiteSpace
  return (CString s1 cmp s2, symTable2)


boolComparison :: SymbolTable -> Parser (Comparison, SymbolTable)
boolComparison symTable = do
  (s1, symTable1) <- boolVal symTable
  whiteSpace
  cmp <- boolComparator
  whiteSpace
  (s2, symTable2) <- boolVal symTable1
  whiteSpace
  return (CBool s1 cmp s2, symTable2)


taskComparison :: SymbolTable -> Parser (Comparison, SymbolTable)
taskComparison symTable = do
  (s1, symTable1) <- task' symTable
  whiteSpace
  cmp <- boolComparator
  whiteSpace
  (s2, symTable2) <- task' symTable1
  whiteSpace
  return (CTask s1 cmp s2, symTable2)


memberComparison :: SymbolTable -> Parser (Comparison, SymbolTable)
memberComparison symTable = do
  (s1, symTable1) <- member symTable
  whiteSpace
  cmp <- boolComparator
  whiteSpace
  (s2, symTable2) <- member symTable1
  whiteSpace
  return (CMember s1 cmp s2, symTable2)


cycle' :: SymbolTable -> Parser (Cycle, SymbolTable)
cycle' symTable = mapCycle symTable


mapCycle :: SymbolTable -> Parser (Cycle, SymbolTable)
mapCycle symTable = do
  whiteSpace
  reserved "map"
  whiteSpace
  reservedOp "("
  whiteSpace
  i <- identifier
  reservedOp ","
  (l, symTable1) <- mapList symTable
  whiteSpace
  reservedOp ")"
  whiteSpace
  return (Cycle { mapF = i, mapL = l }, symTable1)

mapList :: SymbolTable -> Parser (CycleList, SymbolTable)
mapList symTable =
  try (do
    (list, symTable1) <- lists symTable
    return (CycleList list, symTable1))
  <|> try (do
    ident <- identifier
    return (CycleId ident, symTable))

---

funcPattern :: SymbolTable -> Parser (FuncBody, SymbolTable)
funcPattern symTable = do
  whiteSpace
  reserved "pattern"
  whiteSpace
  reservedOp "{"
  whiteSpace
  (c, symTable1) <- casePatterns symTable
  whiteSpace
  (d, symTable2) <- defaultPattern symTable1
  whiteSpace
  reservedOp "}"
  whiteSpace
  return (FuncPattern c d, symTable2)


casePatterns :: SymbolTable -> Parser ([PatternCase], SymbolTable)
casePatterns symTable = manyAccum casePattern symTable
  where
    manyAccum p s = do
      res <- optionMaybe (p s)
      case res of
        Nothing -> return ([], s)
        Just (x, s1) -> do
          (xs, s2) <- manyAccum p s1
          return (x:xs, s2)


casePattern :: SymbolTable -> Parser (PatternCase, SymbolTable)
casePattern symTable = do
  whiteSpace
  reserved "case"
  whiteSpace
  reservedOp "("
  whiteSpace
  (c, symTable1) <- casePatternVals symTable
  whiteSpace
  reservedOp ")"
  whiteSpace
  reservedOp "{"
  whiteSpace
  (s, symTable2) <- statement symTable1
  whiteSpace
  reservedOp "}"
  whiteSpace
  return (Case c s, symTable2)


defaultPattern :: SymbolTable -> Parser (PatternDefault, SymbolTable)
defaultPattern symTable = do
  whiteSpace
  reserved "default"
  whiteSpace
  reservedOp "{"
  whiteSpace
  (s, symTable1) <- statement symTable
  whiteSpace
  reservedOp "}"
  whiteSpace
  return (PDefault s, symTable1)


casePatternVal :: SymbolTable -> Parser (PatternCaseValue, SymbolTable)
casePatternVal symTable =
  try (casePatternEmpty symTable)
  <|> try (casePatternValue symTable)


casePatternEmpty :: SymbolTable -> Parser (PatternCaseValue, SymbolTable)
casePatternEmpty symTable = do
  whiteSpace
  _ <- string "_"
  whiteSpace
  return (PCaseEmpty, symTable)


casePatternValue :: SymbolTable -> Parser (PatternCaseValue, SymbolTable)
casePatternValue symTable = do
  whiteSpace
  (v, symTable1) <- value' symTable
  whiteSpace
  return (PCaseValue v, symTable1)


casePatternVals :: SymbolTable -> Parser ([PatternCaseValue], SymbolTable)
casePatternVals symTable = sepByAccum casePatternVal symTable
  where
    sepByAccum p s = do
      res <- optionMaybe (p s)
      case res of
        Nothing -> return ([], s)
        Just (x, s1) -> do
          _ <- optionMaybe (char ',')
          (xs, s2) <- sepByAccum p s1
          return (x:xs, s2)


funcCall :: SymbolTable -> Parser (FuncCall, SymbolTable)
funcCall symTable = do
  whiteSpace
  i <- identifier
  whiteSpace
  _ <- string "("
  whiteSpace
  (p, symTable1) <- funcCallParams symTable
  whiteSpace
  _ <- string ")"
  return (FuncCall i p, symTable1)


funcCallParams :: SymbolTable -> Parser ([FuncCallParam], SymbolTable)
funcCallParams symTable = sepByAccum funcCallParam symTable
  where
    sepByAccum p s = do
      res <- optionMaybe (p s)
      case res of
        Nothing -> return ([], s)
        Just (x, s1) -> do
          _ <- optionMaybe (char ',')
          (xs, s2) <- sepByAccum p s1
          return (x:xs, s2)


funcCallParam :: SymbolTable -> Parser (FuncCallParam, SymbolTable)
funcCallParam symTable =
  try (funcCallParamVal symTable)
  <|> try (funcCallParamFC symTable)


funcCallParamVal :: SymbolTable -> Parser (FuncCallParam, SymbolTable)
funcCallParamVal symTable = do
  whiteSpace
  (v, symTable1) <- value' symTable
  whiteSpace
  return (FCParamValue v, symTable1)


funcCallParamFC :: SymbolTable -> Parser (FuncCallParam, SymbolTable)
funcCallParamFC symTable = do
  whiteSpace
  (v, symTable1) <- funcCall symTable
  whiteSpace
  return (FCParam v, symTable1)

