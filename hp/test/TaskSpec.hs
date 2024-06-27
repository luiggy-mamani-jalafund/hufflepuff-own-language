{-# LANGUAGE OverloadedStrings #-}

module TaskSpec (spec) where

import Test.Hspec
import AbstractSyntaxTree
import HpParser (task')
import SymbolTable (emptyTable)
import Text.Parsec (parse)

spec :: Spec
spec = do
  describe "Task Parser" $ do
    it "parses a simple task" $ do
      let input = "Task { title: \"My Task\", description: \"This is a test task\", state: \"open\", members: ListMember [], tag: noTag, subTasks: ListTask [] }"
      let expectedTask = Task
            { title = TaskValueTitle (String "My Task")
            , description = TaskValueDescription (String "This is a test task")
            , state = TaskValueState (StringId "open")
            , members = TaskValueMembers (ListMember [])
            , tag = TaskValueTag NoTag
            , subTasks = TaskValueSubTasks (ListTask [])
            }
      let result = parse (task' emptyTable) "" input
      case result of
        Right (task, _) -> task `shouldBe` expectedTask
        Left err -> expectationFailure (show err)

    it "parses a task with identifier titles" $ do
      let input = "Task { title: titleId, description: \"Task with identifier title\", state: \"inProgress\", members: ListMember [], tag: noTag, subTasks: ListTask [] }"
      let expectedTask = Task
            { title = TaskIdentifierTitle "titleId"
            , description = TaskValueDescription (String "Task with identifier title")
            , state = TaskValueState (StringId "inProgress")
            , members = TaskValueMembers (ListMember [])
            , tag = TaskValueTag NoTag
            , subTasks = TaskValueSubTasks (ListTask [])
            }
      let result = parse (task' emptyTable) "" input
      case result of
        Right (task, _) -> task `shouldBe` expectedTask
        Left err -> expectationFailure (show err)

    it "parses a task with take attributes" $ do
      let input = "Task { title: takeTitleId, description: takeDescriptionId, state: takeStateId, members: takeMembersId, tag: takeTagId, subTasks: takeSubTasksId }"
      let expectedTask = Task
            { title = TaskIdentifierTitle "takeTitleId"
            , description = TaskIdentifierDescription "takeDescriptionId"
            , state = TaskIdentifierState "takeStateId"
            , members = TaskIdentifierMembers "takeMembersId"
            , tag = TaskIdentifierTag "takeTagId"
            , subTasks = TaskIdentifierSubTasks "takeSubTasksId"
            }
      let result = parse (task' emptyTable) "" input
      case result of
        Right (task, _) -> task `shouldBe` expectedTask
        Left err -> expectationFailure (show err)
