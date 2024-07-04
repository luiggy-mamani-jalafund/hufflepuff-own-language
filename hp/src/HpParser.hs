{-# LANGUAGE FlexibleContexts #-}
{-# HLINT ignore "Use first" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use $>" #-}
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
    strFree,
    parseCode,
  )
where

import AbstractSyntaxTree
import Lexer
import SymbolTable
import Text.Parsec hiding (manyAccum)
import Text.Parsec.String (Parser)
import ErrorHandler (SemanticError(AlreadyDeclared))

parseCode :: Parser (Code, SymbolTable)
parseCode =
  let initialSymbolTable = emptyTable
   in code initialSymbolTable

code :: SymbolTable -> Parser (Code, SymbolTable)
code symTable =
  whiteSpace *> funcs symTable >>= \(f, symTable1) ->
    whiteSpace *> doNotation symTable1 >>= \(d, symTable2) ->
      whiteSpace *> return (Code f d, symTable2)

funcs :: SymbolTable -> Parser ([Func], SymbolTable)
funcs = manyAccum func

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
  let newScopeSymTable = enterScope symTable
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
  (params, symTable') <- funcParams newScopeSymTable
  whiteSpace
  _ <- string "}"
  whiteSpace
  (body, symTable'') <- funcBody symTable'
  whiteSpace
  _ <- string "}"
  whiteSpace

  let mainScope = exitScope symTable''
  case lookupCurrentScope funId mainScope of
    Just _  -> fail $ show (AlreadyDeclared funId)
    Nothing -> do
      let func = Func funId (str2type funType) params body
      let symTable''' = insertFunction funId (str2type funType) params body mainScope
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
  case lookupCurrentScope i symTable of
    Just _ -> fail $ show (AlreadyDeclared i)
    Nothing -> do
      let symTable' = insertFuncParam i (str2type paramType) symTable
      return (param, symTable')

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
          return (x : xs, s2)

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
  try ((\(cond, symTable') -> (SBoolCondition cond, symTable')) <$> condition symTable)
    <|> try ((\(boolExp, symTable') -> (SBoolExp boolExp, symTable')) <$> boolExpression symTable)
    <|> try ((\(val, symTable') -> (SValue val, symTable')) <$> value' symTable)
    <|> try ((\(cycle, symTable') -> (SCycle cycle, symTable')) <$> cycle' symTable)
    <|> try ((\(funcCall, symTable') -> (SFuncCall funcCall, symTable')) <$> funcCall symTable)
    <|> try ((\taskAttr -> (STakeTaskAttribute taskAttr, symTable)) <$> takeTaskAttribute)
    <|> try ((\memberAttr -> (STakeMemberAttribute memberAttr, symTable)) <$> takeMemberAttribute)

value' :: SymbolTable -> Parser (Value, SymbolTable)
value' symTable =
  try ((\literal -> (ValLiteral literal, symTable)) <$> literal)
    <|> try ((\(task, symTable') -> (ValTask task, symTable')) <$> task' symTable)
    <|> try ((\(member, symTable') -> (ValMember member, symTable')) <$> member symTable)
    <|> try ((\(list, symTable') -> (ValList list, symTable')) <$> lists symTable)
    <|> try ((\tag -> (ValTag tag, symTable)) <$> tag')
    <|> try ((\boolVal -> (ValBool boolVal, symTable)) <$> boolValue)

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

  let task =
        Task
          { title = t,
            description = d,
            state = s,
            members = m,
            tag = tg,
            subTasks = st
          }
  let symTable7 = insertTask (show t) task symTable6 
  return (task, symTable7)

taskTitle :: SymbolTable -> Parser (TitleTask, SymbolTable)
taskTitle symTable =
  try ((\t -> (t, symTable)) . TaskTakeTitle <$> takeTaskAttributeTitle)
    <|> try ((\t -> (t, symTable)) . TaskValueTitle . String <$> strFree)
    <|> try ((\t -> (t, symTable)) . TaskIdentifierTitle <$> identifier)

taskDescription :: SymbolTable -> Parser (DescriptionTask, SymbolTable)
taskDescription symTable =
  try ((\d -> (d, symTable)) . TaskTakeDescription <$> takeTaskAttributeDescription)
    <|> try ((\d -> (d, symTable)) . TaskValueDescription . String <$> strFree)
    <|> try ((\d -> (d, symTable)) . TaskIdentifierDescription <$> identifier)

taskState :: SymbolTable -> Parser (StateTask, SymbolTable)
taskState symbTable =
  try ((\s -> (s, symbTable)) . TaskTakeState <$> takeTaskAttributeState)
    <|> try ((\s -> (s, symbTable)) . TaskValueState <$> state')
    <|> try ((\s -> (s, symbTable)) . TaskIdentifierState <$> identifier)

taskMembers :: SymbolTable -> Parser (MembersTask, SymbolTable)
taskMembers symTable =
  try ((\m -> (m, symTable)) . TaskTakeMembers <$> takeTaskAttributeMembers)
    <|> try ((\(l, symTable') -> (TaskValueMembers l, symTable')) <$> listOfMembers symTable)
    <|> try ((\m -> (m, symTable)) <$> (TaskIdentifierMembers <$ whiteSpace <*> identifier <* whiteSpace))

taskTag :: SymbolTable -> Parser (TagTask, SymbolTable)
taskTag symTable =
  try ((\t -> (t, symTable)) . TaskTakeTag <$> takeTaskAttributeTag)
    <|> try ((\t -> (t, symTable)) . TaskValueTag <$> tag')
    <|> try ((\t -> (t, symTable)) . TaskIdentifierTag <$> identifier)

taskSubTasks :: SymbolTable -> Parser (SubTasksTask, SymbolTable)
taskSubTasks symTable =
  try ((\t -> (t, symTable)) . TaskTakeSubTasks <$> takeTaskAttributeSubTasks)
    <|> try ((\(l, symTable') -> (TaskValueSubTasks l, symTable')) <$> lists symTable)
    <|> try ((\t -> (t, symTable)) <$> (TaskIdentifierSubTasks <$ whiteSpace <*> identifier <* whiteSpace))

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
listOfLists symTable =
  (\(l, symTable') -> (ListList l, symTable'))
    <$ whiteSpace
    <* string "List:List"
    <* whiteSpace
    <* string "["
    <* whiteSpace
    <*> sepByAccum lists symTable
    <* whiteSpace
    <* string "]"
    <* whiteSpace

sepByAccum :: (SymbolTable -> Parser (a, SymbolTable)) -> SymbolTable -> Parser ([a], SymbolTable)
sepByAccum p s = do
  res <- optionMaybe (p s)
  case res of
    Nothing -> return ([], s)
    Just (x, s1) -> do
      _ <- optionMaybe (char ',')
      (xs, s2) <- sepByAccum p s1
      return (x : xs, s2)

listOfStrId :: SymbolTable -> Parser (List, SymbolTable)
listOfStrId symTable =
  (\l -> (l, symTable))
    . ListStringId
    <$ whiteSpace
    <* string "List:StringId"
    <* whiteSpace
    <* string "["
    <* whiteSpace
    <*> sepBy (StringId <$> strId) (char ',')
    <* whiteSpace
    <* string "]"
    <* whiteSpace

listOfStrFree :: SymbolTable -> Parser (List, SymbolTable)
listOfStrFree symTable =
  (\l -> (l, symTable))
    . ListString
    <$ whiteSpace
    <* string "List:String"
    <* whiteSpace
    <* string "["
    <* whiteSpace
    <*> sepBy (String <$> strFree) (char ',')
    <* whiteSpace
    <* string "]"
    <* whiteSpace

listOfMembers :: SymbolTable -> Parser (List, SymbolTable)
listOfMembers symTable =
  (\(l, symTable') -> (list l, newSymTable (list l) symTable'))
    <$ whiteSpace
    <* string "List:Member"
    <* whiteSpace
    <* string "["
    <* whiteSpace
    <*> sepByAccum member symTable
    <* whiteSpace
    <* string "]"
    <* whiteSpace
  where
    newSymTable lt symTable' =
      let listId = show lt
      in case lookupSymbol listId symTable' of
           Just _  -> error $ show (AlreadyDeclared listId)
           Nothing -> insertList listId lt symTable'
    list = ListMember

listOfTasks :: SymbolTable -> Parser (List, SymbolTable)
listOfTasks symTable =
  (\(t, symTable') -> (lt t, newSymTable t symTable'))
    <$ whiteSpace
    <* string "List:Task"
    <* whiteSpace
    <* string "["
    <* whiteSpace
    <*> sepByAccum task' symTable
    <* whiteSpace
    <* string "]"
    <* whiteSpace
  where
    lt = ListTask
    newSymTable t = insertList (show t) (lt t)


listOfTags :: SymbolTable -> Parser (List, SymbolTable)
listOfTags symTa =
  (\t -> (lt t, newSymTable t symTa))
    <$ whiteSpace
    <* string "List:Tag"
    <* whiteSpace
    <* string "["
    <* whiteSpace
    <*> sepBy tag' (char ',')
    <* whiteSpace
    <* string "]"
    <* whiteSpace
  where
    lt = ListTag
    newSymTable t symTa' =
      let listId = show (lt t)
      in case lookupSymbol listId symTa' of
           Just _  -> error $ show (AlreadyDeclared listId)
           Nothing -> insertList listId (lt t) symTa'


listOfStates :: SymbolTable -> Parser (List, SymbolTable)
listOfStates symTa =
  (\s -> (ls s, newSymTable s symTa))
    <$ whiteSpace
    <* string "List:State"
    <* whiteSpace
    <* string "["
    <* whiteSpace
    <*> sepBy state' (char ',')
    <* whiteSpace
    <* string "]"
    <* whiteSpace
  where
    ls = ListState
    newSymTable s symTa' =
      let listId = show (ls s)
      in case lookupSymbol listId symTa' of
           Just _  -> error $ show (AlreadyDeclared listId)
           Nothing -> insertList listId (ls s) symTa'


listOfBool :: SymbolTable -> Parser (List, SymbolTable)
listOfBool symTa =
  (\b -> (lb b, newSymTable b symTa))
    <$ whiteSpace
    <* string "List:Bool"
    <* whiteSpace
    <* string "["
    <* whiteSpace
    <*> sepBy boolValue (char ',')
    <* whiteSpace
    <* string "]"
    <* whiteSpace
  where
    lb = ListBool
    newSymTable b symTa' =
      let listId = show (lb b)
      in case lookupSymbol listId symTa' of
           Just _  -> error $ show (AlreadyDeclared listId)
           Nothing -> insertList listId (lb b) symTa'

tag' :: Parser Tag
tag' =
  try (NoTag <$ whiteSpace <* string "NoTag" <* whiteSpace)
    <|> try (Tag . StringId <$> strId)

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
    let membetDef = Member {name = n, role = r}
    let symTable3 = insertMember (extractName n) membetDef symTable2

    return (membetDef, symTable3)
    <|> ( do
            whiteSpace
            _ <- string "NoAssigned"
            whiteSpace
            return (NoAssigned, symTable)
        )

extractName :: MemberName -> String
extractName (MemberValueName (String str)) = "MemberValueName: " ++ str
extractName _ = error "Invalid member name."

takeMemberAttribute :: Parser TakeMemberAttribute
takeMemberAttribute = try tmaName <|> try tmaRole

tmaName :: Parser TakeMemberAttribute
tmaName = TakeMemberAttributeName <$> takeAttributeId ".name"

tmaRole :: Parser TakeMemberAttribute
tmaRole = TakeMemberAttributeRole <$> takeAttributeId ".role"

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

takeAttributeId :: String -> Parser String
takeAttributeId i =
  whiteSpace
    *> identifier
    <* string i
    <* whiteSpace

takeTaskAttributeTitle :: Parser String
takeTaskAttributeTitle = takeAttributeId ".title"

takeTaskAttributeDescription :: Parser String
takeTaskAttributeDescription = takeAttributeId ".description"

takeTaskAttributeState :: Parser String
takeTaskAttributeState = takeAttributeId ".state"

takeTaskAttributeTag :: Parser String
takeTaskAttributeTag = takeAttributeId ".tag"

takeTaskAttributeMembers :: Parser String
takeTaskAttributeMembers = takeAttributeId ".members"

takeTaskAttributeSubTasks :: Parser String
takeTaskAttributeSubTasks = takeAttributeId ".subTasks"

boolComparatorId :: String -> Parser ()
boolComparatorId v = whiteSpace <* string v <* whiteSpace

boolComparator :: Parser BoolComparator
boolComparator =
  try (Eq <$ boolComparatorId "==")
    <|> try (Neq <$ boolComparatorId "!=")
    <|> try (Lt <$ boolComparatorId "<")
    <|> try (Le <$ boolComparatorId "<=")
    <|> try (Gt <$ boolComparatorId ">")
    <|> try (Ge <$ boolComparatorId ">=")
    <|> try (And <$ boolComparatorId "&&")
    <|> try (Or <$ boolComparatorId "||")

memberName :: SymbolTable -> Parser (MemberName, SymbolTable)
memberName symTable =
  try ((\tn -> (MemberTakeName tn, symTable)) <$> takeMemberAttributeName)
    <|> try
      ( ( \v ->
            let symTable' = insertLiteral v (LString (String v)) symTable
             in (MemberValueName (String v), symTable')
        )
          <$> strFree
      )
    <|> try
      ( ( \id ->
            let symTable' = insertVariable id TString Nothing symTable
             in (MemberIdentifierName id, symTable')
        )
          <$> identifier
      )


memberRole :: SymbolTable -> Parser (MemberRole, SymbolTable)
memberRole symTable =
  try ((\tr -> (tr, symTable)) . MemberTakeRole <$> takeMemberAttributeRole)
    <|> try
      ( ( \v ->
            let symTable' = insertLiteral v (LStringIdentifier (StringId v)) symTable
             in (MemberValueRole (StringId v), symTable')
        )
          <$> strId
      )
    <|> try
      ( ( \id ->
            let symTable' = insertVariable id TStringId Nothing symTable
             in (MemberIdentifierRole id, symTable')
        )
          <$> identifier
      )


takeMemberAttributeName :: Parser String
takeMemberAttributeName = takeAttributeId ".name"

takeMemberAttributeRole :: Parser String
takeMemberAttributeRole = takeAttributeId ".role"

literal :: Parser Literal
literal =
  try (LString . String <$> strFree)
    <|> try (LStringIdentifier . StringId <$> strId)
    <|> try (LTakeTaskAttribute <$> takeTaskAttributeStrings)
    <|> try (LTakeMemberAttribute <$> takeMemberAttribute)

strId' :: Parser String
strId' = (:) <$> alphaNum <*> many (alphaNum <|> space)

strId :: Parser String
strId =
  whiteSpace
    *> char '\"'
    *> strId'
    <* char '\"'
    <* whiteSpace

strEmpty :: Parser String
strEmpty =
  ""
    <$ whiteSpace
    <* char '\"'
    <* char '\"'
    <* whiteSpace

strFree :: Parser String
strFree = try strEmpty <|> try strFree'

strFree' :: Parser String
strFree' =
  whiteSpace
    *> char '\"'
    *> many (alphaNum <|> space <|> oneOf symbols)
    <* char '\"'
    <* whiteSpace

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
  let conditionDef = Condition {ifCondition = e, thenStatement = s1, elseStatament = s2}
  return (conditionDef, symTable3)

boolExpression :: SymbolTable -> Parser (BoolExpression, SymbolTable)
boolExpression symTable =
  try
    ( do
        whiteSpace
        (comp, symTable1) <- comparison symTable
        whiteSpace
        let boolExp = BoolComparison comp
        return (boolExp, symTable1)
    )
    <|> try
      ( do
          whiteSpace
          boolVal <- boolValue
          whiteSpace
          let boolExpr = BoolValue boolVal
          case lookupSymbol (show boolExpr) symTable of
            Just _  -> fail $ show (AlreadyDeclared (show boolExpr))
            Nothing ->
              let symTable' = insertBoolExpression (show boolExpr) boolExpr symTable
              in return (boolExpr, symTable')
      )


comparison :: SymbolTable -> Parser (Comparison, SymbolTable)
comparison symTable =
  try ((\r -> (r, symTable)) <$> boolComparison)
    <|> try ((\r -> (r, symTable)) <$> strComparison)
    <|> try (taskComparison symTable)
    <|> try (memberComparison symTable)

boolValue :: Parser Bool
boolValue =
  try (True <$ string "True")
    <|> try (False <$ string "False")

strComparison :: Parser Comparison
strComparison =
  ComparisonString
    <$> literal
    <* whiteSpace
    <*> boolComparator
    <* whiteSpace
    <*> literal
    <* whiteSpace

boolComparison :: Parser Comparison
boolComparison =
  ComparisonBool
    <$> boolValue
    <* whiteSpace
    <*> boolComparator
    <* whiteSpace
    <*> boolValue
    <* whiteSpace

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
  let cycleDef = Cycle {mapF = i, mapL = l}
  case lookupSymbol i symTable1 of
    Just _  -> fail $ show (AlreadyDeclared i)
    Nothing ->
      let symTable2 = insertDoAssignment i TListString (SCycle cycleDef) symTable1
      in return (cycleDef, symTable2)

mapList :: SymbolTable -> Parser (CycleList, SymbolTable)
mapList symTable =
  try
    ((\(list, symTable') -> (CycleList list, symTable')) <$> lists symTable)
    <|> try
      ( do
          id <- identifier
          case lookupSymbol id symTable of
            Just _  -> fail $ show (AlreadyDeclared  id)
            Nothing ->
              let cycleList = CycleId id
                  symTable' = insertVariable id TListString Nothing symTable
              in return (cycleList, symTable')
      )


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
  try ((\val -> (val, symTable)) <$> casePatternEmpty)
    <|> try (casePatternValue symTable)

casePatternEmpty :: Parser PatternCaseValue
casePatternEmpty =
  PatternCaseEmpty
    <$ whiteSpace
    <* string "_"
    <* whiteSpace

casePatternValue :: SymbolTable -> Parser (PatternCaseValue, SymbolTable)
casePatternValue symTable =
  (\(v, symTable') -> (PatternCaseValue v, symTable'))
    <$ whiteSpace
    <*> value' symTable
    <* whiteSpace

casePatternVals :: SymbolTable -> Parser ([PatternCaseValue], SymbolTable)
casePatternVals = sepByAccum casePatternVal

funcCall :: SymbolTable -> Parser (FuncCall, SymbolTable)
funcCall symTable = do
  whiteSpace
  i <- identifier
  whiteSpace
  _ <- string "("
  whiteSpace
  (p, symT1) <- funcCallParams symTable
  whiteSpace
  _ <- string ")"
  return (FuncCall i p, symT1)

funcCallParams :: SymbolTable -> Parser ([FuncCallParam], SymbolTable)
funcCallParams = sepByAccum funcCallParam

funcCallParam :: SymbolTable -> Parser (FuncCallParam, SymbolTable)
funcCallParam symTable =
  try (funcCallParamVal symTable)
    <|> try (funcCallParamFC symTable)
    <|> try (funcCallId symTable)

funcCallParamVal :: SymbolTable -> Parser (FuncCallParam, SymbolTable)
funcCallParamVal symbolTable =
  (\(v, symTable') -> (FuncCallParamValue v, symTable'))
    <$ whiteSpace
    <*> value' symbolTable
    <* whiteSpace


funcCallParamFC :: SymbolTable -> Parser (FuncCallParam, SymbolTable)
funcCallParamFC symTabl =
  (\(v, symTable') -> (FuncCallParam v, symTable'))
    <$ whiteSpace
    <*> funcCall symTabl
    <* whiteSpace

funcCallId :: SymbolTable -> Parser (FuncCallParam, SymbolTable)
funcCallId symTab =
  (\v -> (FuncCallIdentifier v, symTab))
    <$ whiteSpace
    <*> identifier
    <* whiteSpace

doNotation :: SymbolTable -> Parser (DoNotation, SymbolTable)
doNotation symTable =
  (\(v, symTable') -> (DoNotation v, symTable'))
    <$ reserved "do"
    <* whiteSpace
    <* reservedOp "{"
    <* whiteSpace
    <*> doStatements symTable
    <* whiteSpace
    <* reservedOp "}"
    <* whiteSpace

doStatements :: SymbolTable -> Parser ([DoStatement], SymbolTable)
doStatements = manyAccum doStatement

doStatement :: SymbolTable -> Parser (DoStatement, SymbolTable)
doStatement symTable =
  try (doAssignment symTable)
    <|> try (doPrint symTable)

doAssignment :: SymbolTable -> Parser (DoStatement, SymbolTable)
doAssignment symTable = do
  whiteSpace
  reserved "let"
  whiteSpace
  i <- identifier
  _ <- string ":"
  t <- dataType
  whiteSpace
  _ <- string "="
  whiteSpace
  (s, symTable1) <- statement symTable
  whiteSpace
  let assignment = DoAssignment i (str2type t) s
  case lookupSymbol i symTable1 of
    Just _  -> fail $ show (AlreadyDeclared i)
    Nothing ->
      let symTable2 = insertDoAssignment i (str2type t) s symTable1
      in return (assignment, symTable2)


doPrint :: SymbolTable -> Parser (DoStatement, SymbolTable)
doPrint symTable =
  (\(v, symTable') -> (DoPrint v, symTable'))
    <$ whiteSpace
    <* reserved "print"
    <* whiteSpace
    <* string "("
    <* whiteSpace
    <*> (try (printStatement symTable) <|> try (printRef symTable))
    <* whiteSpace
    <* string ")"
    <* whiteSpace

printStatement :: SymbolTable -> Parser (Print, SymbolTable)
printStatement symTable =
  (\(s, symTable') -> (PrintStatement s, symTable'))
    <$> statement symTable

printRef :: SymbolTable -> Parser (Print, SymbolTable)
printRef symTable = do
  (\i -> (PrintRef i, symTable)) <$> identifier

str2type :: String -> Type
str2type str = read $ 'T' : strType
  where
    strType = filter (/= ':') str