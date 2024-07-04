-- File: test/TaskSpec.hs
{-# LANGUAGE OverloadedStrings #-}

module TaskSpec (spec) where

import Test.Hspec
import Text.Parsec (parse)
import HpParser
import AbstractSyntaxTree
import SymbolTable (emptyTable)
import Lexer (whiteSpace)

spec :: Spec
spec = do
  describe "Task Parser" $ do

    it "parses Task with TaskValueTitle" $ do
      let input = "Task { title: \"Title\", description: \"Description\", state: \"State\", members: List:Member[], tag: NoTag, subTasks: List:Task[] }"
          result = parse (whiteSpace *> task' emptyTable) "" input
          expectedTask = Task 
            { title = TaskValueTitle (String "Title")
            , description = TaskValueDescription (String "Description")
            , state = TaskValueState (StringId "State")
            , members = TaskValueMembers (ListMember [])
            , tag = TaskValueTag NoTag
            , subTasks = TaskValueSubTasks (ListTask [])
            }
      case result of
        Right (task, _) -> task `shouldBe` expectedTask
        _               -> expectationFailure "Parsing failed"

    it "parses Task with TaskIdentifierTitle" $ do
      let input = "Task { title: titleId, description: descriptionId, state: stateId, members: membersId, tag: tagId, subTasks: subTasksId }"
          result = parse (whiteSpace *> task' emptyTable) "" input
          expectedTask = Task 
            { title = TaskIdentifierTitle "titleId"
            , description = TaskIdentifierDescription "descriptionId"
            , state = TaskIdentifierState "stateId"
            , members = TaskIdentifierMembers "membersId"
            , tag = TaskIdentifierTag "tagId"
            , subTasks = TaskIdentifierSubTasks "subTasksId"
            }
      case result of
        Right (task, _) -> task `shouldBe` expectedTask
        _               -> expectationFailure "Parsing failed"

    it "parses Task with TaskTakeTitle" $ do
      let input = "Task { title: takeTitle.title, description: takeDescription.description, state: takeState.state, members: takeMembers.members, tag: takeTag.tag, subTasks: takeSubTasks.subTasks }"
          result = parse (whiteSpace *> task' emptyTable) "" input
          expectedTask = Task 
            { title = TaskTakeTitle "takeTitle"
            , description = TaskTakeDescription "takeDescription"
            , state = TaskTakeState "takeState"
            , members = TaskTakeMembers "takeMembers"
            , tag = TaskTakeTag "takeTag"
            , subTasks = TaskTakeSubTasks "takeSubTasks"
            }
      case result of
        Right (task, _) -> task `shouldBe` expectedTask
        _               -> expectationFailure "Parsing failed"

    it "parses Task with mixed attributes" $ do
      let input = "Task { title: \"Mixed Title\", description: descriptionId, state: takeState.state, members: List:Member[], tag: NoTag, subTasks: List:Task[] }"
          result = parse (whiteSpace *> task' emptyTable) "" input
          expectedTask = Task 
            { title = TaskValueTitle (String "Mixed Title")
            , description = TaskIdentifierDescription "descriptionId"
            , state = TaskTakeState "takeState"
            , members = TaskValueMembers (ListMember [])
            , tag = TaskValueTag NoTag
            , subTasks = TaskValueSubTasks (ListTask [])
            }
      case result of
        Right (task, _) -> task `shouldBe` expectedTask
        _               -> expectationFailure "Parsing failed"

    it "parses Task with empty lists" $ do
      let input = "Task { title: titleId, description: descriptionId, state: stateId, members: List:Member[], tag: NoTag, subTasks: List:Task[] }"
          result = parse (whiteSpace *> task' emptyTable) "" input
          expectedTask = Task 
            { title = TaskIdentifierTitle "titleId"
            , description = TaskIdentifierDescription "descriptionId"
            , state = TaskIdentifierState "stateId"
            , members = TaskValueMembers (ListMember [])
            , tag = TaskValueTag NoTag
            , subTasks = TaskValueSubTasks (ListTask [])
            }
      case result of
        Right (task, _) -> task `shouldBe` expectedTask
        _               -> expectationFailure "Parsing failed"

    it "fails to parse Task with missing attributes" $ do
      let input = "Task { title: titleId, description: descriptionId, state: stateId }"
          result = parse (whiteSpace *> task' emptyTable) "" input
      case result of
        Left _  -> return () -- Expected to fail
        Right _ -> expectationFailure "Parsing should have failed"
