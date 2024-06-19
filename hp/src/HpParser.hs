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
import SymbolTable

parseCode :: Parser (Code, SymbolTable)
parseCode = do
  let initialSymbolTable = emptyTable
  code initialSymbolTable

code :: SymbolTable -> Parser (Code, SymbolTable)
code symTable = do
  whiteSpace
  (f, symTable1) <- funcs symTable
  whiteSpace
  (d, symTable2) <- doNotation symTable1
  whiteSpace
  return (Code f d, symTable2)

funcs :: SymbolTable -> Parser ([Func], SymbolTable)
funcs symTable = do
  (fs, symTable') <- manyAccum func symTable
  return (fs, symTable')

manyAccum :: (SymbolTable -> Parser (a, SymbolTable)) -> SymbolTable -> Parser ([a], SymbolTable)
manyAccum p symTable = do
  result <- optionMaybe (p symTable)
  case result of
    Nothing -> return ([], symTable)
    Just (x, symTable') -> do
      (xs, symTable'') <- manyAccum p symTable'
      return (x : xs, symTable'')

func :: SymbolTable -> Parser (Func, SymbolTable)
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
  (params, symTable') <- funcParams symTable
  whiteSpace
  _ <- string "}"
  whiteSpace
  (body, symTable'') <- funcBody symTable'
  whiteSpace
  _ <- string "}"
  whiteSpace
  let func = Func funId (str2type funType) params body
  let symTable''' = insertFunction funId (str2type funType) params body symTable''
  return (func, symTable''')

funcParam :: SymbolTable -> Parser (FuncParam, SymbolTable)
funcParam symTable = do
  whiteSpace
  i <- identifier
  whiteSpace
  reservedOp ":"
  whiteSpace
  paramType <- dataType
  whiteSpace
  let param = FuncParam i (str2type paramType)
  return (param, symTable)

funcParams :: SymbolTable -> Parser ([FuncParam], SymbolTable)
funcParams symTable = do
  (params, symTable') <- sepByAccum funcParam symTable
  return (params, symTable')
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
  (s, symTable') <- statement symTable
  whiteSpace
  reservedOp "}"
  return (FuncReturn s, symTable')

statement :: SymbolTable -> Parser (Statement, SymbolTable)
statement symTable = 
  try (do
    (cond, symTable') <- condition symTable
    return (SBoolCondition cond, symTable'))
  <|> try (do
    (boolExp, symTable') <- boolExpression symTable
    return (SBoolExp boolExp, symTable'))
  <|> try (do
    (val, symTable') <- value' symTable
    return (SValue val, symTable'))
  <|> try (do
    (cycle, symTable') <- cycle' symTable
    return (SCycle cycle, symTable'))
  <|> try (do
    (funcCall, symTable') <- funcCall symTable
    return (SFuncCall funcCall, symTable'))
  <|> try (do
    taskAttr <- takeTaskAttribute
    return (STakeTaskAttribute taskAttr, symTable))
  <|> try (do
    memberAttr <- takeMemberAttribute
    return (STakeMemberAttribute memberAttr, symTable))

value' :: SymbolTable -> Parser (Value, SymbolTable)
value' symTable = 
  try (do
    literal <- literal
    return (ValLiteral literal, symTable))
  <|> try (do
    (task, symTable') <- task' symTable
    return (ValTask task, symTable'))
  <|> try (do
    (member, symTable') <- member symTable
    return (ValMember member, symTable'))
  <|> try (do
    (list, symTable') <- lists symTable
    return (ValList list, symTable'))
  <|> try (do
    tag <- tag' 
    return (ValTag tag, symTable))
  <|> try (do
    boolVal <- boolValue 
    return (ValBool boolVal, symTable))

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
  let task = Task { title = t, 
                    description = d, 
                    state = s, 
                    members = m, 
                    tag = tg, 
                    subTasks = st }
  let symTable7 = insertTask (show t) task symTable6 -- Define identifier type in future
  return (task, symTable7)

taskTitle :: SymbolTable -> Parser (TitleTask, SymbolTable)
taskTitle symTable =
  try (do 
    t <- takeTaskAttributeTitle
    return (TaskTakeTitle t, symTable))
    <|> try (do
      srt <- strFree 
      return (TaskValueTitle (String srt), symTable))
    <|> try (do
      id <- TaskIdentifierTitle <$> identifier
      return (id, symTable))

taskDescription :: SymbolTable -> Parser (DescriptionTask, SymbolTable)
taskDescription symTable =
  try (do
    d <- takeTaskAttributeDescription 
    return (TaskTakeDescription d, symTable))
    <|> try (do
      str <- strFree
      return (TaskValueDescription (String str), symTable))
    <|> try (do
      id <- TaskIdentifierDescription <$> identifier
      return (id, symTable))

taskState :: SymbolTable -> Parser (StateTask, SymbolTable)
taskState symbTable =
  try (do 
    s <- takeTaskAttributeState 
    return (TaskTakeState s, symbTable))
    <|> try (do
      state <- state'
      return (TaskValueState state, symbTable))
    <|> try (do
      id <- TaskIdentifierState <$> identifier
      return (id, symbTable))

taskMembers :: SymbolTable -> Parser (MembersTask, SymbolTable)
taskMembers symTable =
  try (do
    attrib <- takeTaskAttributeMembers
    return (TaskTakeMembers attrib, symTable))
    <|> try (do 
      (list, symTable') <- listOfMembers symTable
      return (TaskValueMembers list, symTable'))
    <|> try
      ( do
        whiteSpace
        ident <- TaskIdentifierMembers <$> identifier
        whiteSpace
        return (ident, symTable)
      )

taskTag :: SymbolTable -> Parser (TagTask, SymbolTable)
taskTag symTable =
  try (do
    attrib <- takeTaskAttributeTag 
    return (TaskTakeTag attrib, symTable))
    <|> try (do
      tag <- tag'
      return (TaskValueTag tag, symTable))
    <|> try (do
      id <- TaskIdentifierTag <$> identifier
      return (id, symTable))

taskSubTasks :: SymbolTable -> Parser (SubTasksTask, SymbolTable)
taskSubTasks symTable =
  try (do
    attrib <- takeTaskAttributeSubTasks 
    return (TaskTakeSubTasks attrib, symTable))
    <|> try (do
      (list, symTable') <- lists symTable
      return (TaskValueSubTasks list, symTable'))
    <|> try
      ( do
        whiteSpace
        id <- TaskIdentifierSubTasks <$> identifier
        whiteSpace
        return (id, symTable)
      )

lists :: SymbolTable -> Parser (List, SymbolTable)
lists symTable =
  try (listOfTasks symTable)
  <|> try (listOfMembers symTable)
  <|> try (listOfStates symTable)
  <|> try (listOfTags symTable)
  <|> try (listOfStrId symTable)
  <|> try (listOfStrFree symTable)
  <|> try (listOfBool symTable)
  <|> try (listOfLists symTable)

listOfLists :: SymbolTable -> Parser (List, SymbolTable)
listOfLists symTable = do
  whiteSpace
  _ <- string "List:List"
  whiteSpace
  _ <- string "["
  whiteSpace
  (i, symTable') <- sepByAccum lists symTable
  whiteSpace
  _ <- string "]"
  whiteSpace
  return (ListList i, symTable')

sepByAccum :: (SymbolTable -> Parser (a, SymbolTable)) -> SymbolTable -> Parser ([a], SymbolTable)
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
  i <- sepBy (StringId <$> strId) (char ',')
  whiteSpace
  _ <- string "]"
  whiteSpace
  return (ListStringId i, symTable)


listOfStrFree :: SymbolTable -> Parser (List, SymbolTable)
listOfStrFree symTable = do
  whiteSpace
  _ <- string "List:String"
  whiteSpace
  _ <- string "["
  whiteSpace
  i <- sepBy (String <$> strFree) (char ',')
  whiteSpace
  _ <- string "]"
  whiteSpace
  return (ListString i, symTable)

listOfMembers :: SymbolTable -> Parser (List, SymbolTable)
listOfMembers symTable = do
  whiteSpace
  _ <- string "List:Member"
  whiteSpace
  _ <- string "["
  whiteSpace
  (members, symTable') <- sepByAccum member symTable
  whiteSpace
  _ <- string "]"
  whiteSpace
  let list = ListMember members
  let symTable'' = insertList (show list) list symTable'
  return (list, symTable'')

listOfTasks :: SymbolTable -> Parser (List, SymbolTable)
listOfTasks symTable = do
  whiteSpace
  _ <- string "List:Task"
  whiteSpace
  _ <- string "["
  whiteSpace
  (tasks, symTable') <- sepByAccum task' symTable
  whiteSpace
  _ <- string "]"
  whiteSpace
  let symTable'' = insertList (show tasks) (ListTask tasks) symTable'
  return (ListTask tasks, symTable'')

listOfTags :: SymbolTable -> Parser (List, SymbolTable)
listOfTags symTa = do
  whiteSpace
  _ <- string "List:Tag"
  whiteSpace
  _ <- string "["
  whiteSpace
  tags <- sepBy tag' (char ',')
  whiteSpace
  _ <- string "]"
  whiteSpace
  let symTable'' = insertList (show tags) (ListTag tags) symTa
  return (ListTag tags, symTable'')

listOfStates :: SymbolTable -> Parser (List, SymbolTable)
listOfStates symT = do
  whiteSpace
  _ <- string "List:State"
  whiteSpace
  _ <- string "["
  whiteSpace
  states <- sepBy state' (char ',')
  whiteSpace
  _ <- string "]"
  whiteSpace
  let stateL = ListState states
  let symTable'' = insertList (show stateL) stateL symT
  return (stateL, symTable'')

listOfBool :: SymbolTable -> Parser (List, SymbolTable)
listOfBool symTable = do
  whiteSpace
  _ <- string "List:Bool"
  whiteSpace
  _ <- string "["
  whiteSpace
  boold <- sepBy boolValue (char ',')
  whiteSpace
  _ <- string "]"
  whiteSpace
  let listBo = ListBool boold
  let symTable'' = insertList (show listBo) listBo symTable
  return (ListBool boold, symTable'')

-- no adaptar
tag' :: Parser Tag
tag' =
  try $
    Tag . StringId <$> strId
      <|> do
        whiteSpace
        _ <- string "NoTag"
        whiteSpace
        return NoTag

-- no adaptar
state' :: Parser TaskState
state' = StringId <$> strId

member :: SymbolTable -> Parser (Member, SymbolTable)
member symTable =
  do
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
    let membetDef = Member { name = n, role = r }
    let symTable3 = insertMember (show n) membetDef symTable2

    return (membetDef, symTable3)
    <|> (do
      whiteSpace
      _ <- string "NoAssigned"
      whiteSpace
      return (NoAssigned, symTable))

-- No es necesario adaptar
takeMemberAttribute :: Parser TakeMemberAttribute
takeMemberAttribute =
  try tmaName
    <|> try tmaRole

-- no adaptar
tmaName :: Parser TakeMemberAttribute
tmaName = do
  whiteSpace
  i <- identifier
  _ <- string ".name"
  whiteSpace
  return $ TakeMemberAttributeName i

-- no adaptar
tmaRole :: Parser TakeMemberAttribute
tmaRole = do
  whiteSpace
  i <- identifier
  _ <- string ".role"
  whiteSpace
  return $ TakeMemberAttributeRole i

-- No adaptar
takeTaskAttribute :: Parser TakeTaskAttribute
takeTaskAttribute =
  try (TakeTaskAttributeStrings <$> takeTaskAttributeStrings)
    <|> try (TakeTaskAttributeMembers <$> takeTaskAttributeMembers)
    <|> try (TakeTaskAttributeSubTasks <$> takeTaskAttributeSubTasks)

-- No adaptar
takeTaskAttributeStrings :: Parser TakeTaskAttributeLiteral
takeTaskAttributeStrings =
  try (TakeTaskAttributeState <$> takeTaskAttributeState)
    <|> try (TakeTaskAttributeTitle <$> takeTaskAttributeTitle)
    <|> try (TakeTaskAttributeDescription <$> takeTaskAttributeDescription)
    <|> try (TakeTaskAttributeTag <$> takeTaskAttributeTag)

-- No adaptar
takeTaskAttributeTitle :: Parser String
takeTaskAttributeTitle = do
  whiteSpace
  i <- identifier
  _ <- string ".title"
  whiteSpace
  return i

-- No adaptar
takeTaskAttributeDescription :: Parser String
takeTaskAttributeDescription = do
  whiteSpace
  i <- identifier
  _ <- string ".description"
  whiteSpace
  return i

-- No adaptar
takeTaskAttributeState :: Parser String
takeTaskAttributeState = do
  whiteSpace
  i <- identifier
  _ <- string ".state"
  whiteSpace
  return i

-- no adaptar
takeTaskAttributeTag :: Parser String
takeTaskAttributeTag = do
  whiteSpace
  i <- identifier
  _ <- string ".tag"
  whiteSpace
  return i

-- No adaptar
takeTaskAttributeMembers :: Parser String
takeTaskAttributeMembers = do
  whiteSpace
  i <- identifier
  _ <- string ".members"
  whiteSpace
  return i

-- No adaptar
takeTaskAttributeSubTasks :: Parser String
takeTaskAttributeSubTasks = do
  whiteSpace
  i <- identifier
  _ <- string ".subTasks"
  whiteSpace
  return i

-- No adaptar
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

memberName :: SymbolTable -> Parser (MemberName, SymbolTable)
memberName symTable =
  try (do
    memberTN <- takeMemberAttributeName
    return (MemberTakeName memberTN, symTable)
    )
  <|>
    try (do
      str <- strFree
      let membetName = MemberValueName (String str)
      let symTable' = insertLiteral str (LString (String str)) symTable
      return (membetName, symTable')  
    )
    <|> try (do
      id <- identifier
      let symTable' = insertVariable id TString Nothing symTable
      return (MemberIdentifierName id, symTable')
      )

memberRole :: SymbolTable -> Parser (MemberRole, SymbolTable)
memberRole symTable =
  try (do
    memberTR <- MemberTakeRole <$> takeMemberAttributeRole
    return (memberTR, symTable)
    )
  <|>
    try (do 
      str <- strId
      let memberRole = MemberValueRole (StringId str)
      let symbolTable' = insertLiteral str (LStringIdentifier (StringId str)) symTable
      return (memberRole, symbolTable')
      )
    <|> try (do
      id <- identifier
      let memberRole = MemberIdentifierRole id
      let symbolTable' = insertVariable id TStringId Nothing symTable 
      return (memberRole, symbolTable'))

-- No adaptar
takeMemberAttributeName :: Parser String
takeMemberAttributeName = do
  whiteSpace
  i <- identifier
  _ <- string ".name"
  whiteSpace
  return i

-- No adaptar
takeMemberAttributeRole :: Parser String
takeMemberAttributeRole = do
  whiteSpace
  i <- identifier
  _ <- string ".role"
  whiteSpace
  return i

-- No adaptar
literal :: Parser Literal
literal =
  try (LString . String <$> strFree)
    <|> try (LStringIdentifier . StringId <$> strId)
    <|> try (LTakeTaskAttribute <$> takeTaskAttributeStrings)
    <|> try (LTakeMemberAttribute <$> takeMemberAttribute)

-- No adaptar
strId' :: Parser String
strId' = do
  l <- alphaNum
  v <- many (alphaNum <|> space)
  return $ l : v

-- No adaptar
strId :: Parser String
strId = do
  whiteSpace
  _ <- char '\"'
  v <- strId'
  _ <- char '\"'
  whiteSpace
  return v

-- No adaptar
strEmpty :: Parser String
strEmpty = do
  whiteSpace
  _ <- char '\"'
  _ <- char '\"'
  whiteSpace
  return ""

-- No adaptar
strFree :: Parser String
strFree = try strEmpty <|> try strFree'

-- No adaptar
strFree' :: Parser String
strFree' = do
  whiteSpace
  _ <- char '\"'
  v <- many (alphaNum <|> space <|> oneOf symbols)
  _ <- char '\"'
  whiteSpace
  return v

-- No adaptar
str2type :: String -> Type
str2type str = read $ 'T' : strType
  where
    strType = filter (/= ':') str

condition :: SymbolTable -> Parser (Condition, SymbolTable)
condition symTable = do
  reserved "if"
  whiteSpace
  _ <- string "("
  whiteSpace
  (e, symTable1) <- boolExpression symTable
  whiteSpace
  _ <- string ")"
  whiteSpace

  reserved "then"
  whiteSpace
  (s1, symTable2) <- statement symTable1
  whiteSpace
  reserved "else"
  whiteSpace
  (s2, symTable3) <- statement symTable2
  whiteSpace
  let conditionDef = Condition { ifCondition = e, thenStatement = s1, elseStatament = s2 }
  return (conditionDef, symTable3)

boolExpression :: SymbolTable -> Parser (BoolExpression, SymbolTable)
boolExpression symTable =
  try (do
    whiteSpace
    (comp, symTable1) <- comparison symTable
    whiteSpace
    let boolExp = BoolComparison comp
    return (boolExp, symTable1))
    <|> try (do
      whiteSpace
      boolVal <- boolValue
      whiteSpace
      let boolExpr = BoolValue boolVal
      let symTable' = insertBoolExpression (show boolExpr) boolExpr symTable
      return (boolExpr, symTable'))

comparison :: SymbolTable -> Parser (Comparison, SymbolTable)
comparison symTable =
  try (do
    bolcomp <- boolComparison
    return (bolcomp, symTable))
    <|> try (do
      strComp <- strComparison
      return (strComp, symTable))
    <|> try (do
      (taskComp, symTable') <- taskComparison symTable
      return (taskComp, symTable'))
    <|> try (do
      (memberComp, symTable') <- memberComparison symTable
      return (memberComp, symTable'))

-- No adaptar
boolValue :: Parser Bool
boolValue =
  try (True <$ string "True")
    <|> try (False <$ string "False")

-- No adaptar
strComparison :: Parser Comparison
strComparison = do
  s1 <- literal
  whiteSpace
  cmp <- boolComparator
  whiteSpace
  s2 <- literal
  whiteSpace
  return $ ComparisonString s1 cmp s2

-- No adaptar
boolComparison :: Parser Comparison
boolComparison = do
  s1 <- boolValue
  whiteSpace
  cmp <- boolComparator
  whiteSpace
  s2 <- boolValue
  whiteSpace
  return $ ComparisonBool s1 cmp s2

taskComparison :: SymbolTable -> Parser (Comparison, SymbolTable)
taskComparison symTable = do
  (s1, symTable1) <- task' symTable
  whiteSpace
  cmp <- boolComparator
  whiteSpace
  (s2, symTable2) <- task' symTable1
  whiteSpace
  return (ComparisonTask s1 cmp s2, symTable2)

memberComparison :: SymbolTable -> Parser (Comparison, SymbolTable)
memberComparison symTable = do
  (s1, symTable1) <- member symTable
  whiteSpace
  cmp <- boolComparator
  whiteSpace
  (s2, symTable2) <- member symTable1
  whiteSpace
  return (ComparisonMember s1 cmp s2, symTable2)

cycle' :: SymbolTable -> Parser (Cycle, SymbolTable)
cycle' = mapCycle 

mapCycle :: SymbolTable -> Parser (Cycle, SymbolTable)
mapCycle symTable = do
  whiteSpace
  reserved "map"
  whiteSpace
  _ <- string "("
  whiteSpace
  i <- identifier
  reservedOp ","
  (l, symTable1) <- mapList symTable
  whiteSpace
  _ <- string ")"
  whiteSpace
  let cycleDef = Cycle { mapF = i, mapL = l }
  let symTable2 = insertDoAssignment i TListString (SCycle cycleDef) symTable1
  return (cycleDef, symTable2)

mapList :: SymbolTable -> Parser (CycleList, SymbolTable)
mapList symTable =
  try (do
    (list, symTable') <- lists symTable
    return (CycleList list, symTable'))
    <|> try (do
      id <- identifier
      let cycleList = CycleId id
      let symTable' = insertVariable id TListString Nothing symTable
      return (cycleList, symTable'))

funcPattern :: SymbolTable -> Parser (FuncBody, SymbolTable)
funcPattern symTable = do
  whiteSpace
  reserved "pattern"
  whiteSpace
  reservedOp "{"
  whiteSpace
  (c, simTable1) <- casePatterns symTable
  whiteSpace
  (d, symTable2) <- defaultPattern simTable1
  whiteSpace
  reservedOp "}"
  whiteSpace
  return (FuncPattern c d, symTable2)

casePatterns :: SymbolTable -> Parser ([PatternCase], SymbolTable)
casePatterns = manyAccum casePattern

casePattern :: SymbolTable -> Parser (PatternCase, SymbolTable)
casePattern symTable = do
  whiteSpace
  reserved "case"
  whiteSpace
  _ <- string "("
  whiteSpace
  (c, symTable1) <- casePatternVals symTable
  whiteSpace
  _ <- string ")"
  whiteSpace
  _ <- string "{"
  whiteSpace
  (s, symTable2) <- statement symTable1
  whiteSpace
  _ <- string "}"
  whiteSpace
  return (PatternCase c s, symTable2)

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
  return (PatternDefault s, symTable1)

casePatternVal :: SymbolTable -> Parser (PatternCaseValue, SymbolTable)
casePatternVal symTable =
  try (do 
    val <- casePatternEmpty
    return (val, symTable))
    <|> try (do
    (val, symTable') <- casePatternValue symTable
    return (val, symTable'))

-- No adaptar
casePatternEmpty :: Parser PatternCaseValue
casePatternEmpty = do
  whiteSpace
  _ <- string "_"
  whiteSpace
  return PatternCaseEmpty

casePatternValue :: SymbolTable -> Parser (PatternCaseValue, SymbolTable)
casePatternValue symTable = do
  whiteSpace
  (v, symTable') <- value' symTable
  whiteSpace
  return (PatternCaseValue v, symTable')

casePatternVals :: SymbolTable -> Parser ([PatternCaseValue], SymbolTable)
casePatternVals = sepByAccum casePatternVal

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