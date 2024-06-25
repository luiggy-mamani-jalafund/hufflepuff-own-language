{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Visitor (Visitor(..)) where

import AbstractSyntaxTree

class Visitor v a where
  visitLiteral :: v -> Literal -> a
  visitStringIdentifier :: v -> StringIdentifier -> a
  visitStringFree :: v -> StringFree -> a
  visitType :: v -> Type -> a
  visitValue :: v -> Value -> a
  visitTag :: v -> Tag -> a
  visitTask :: v -> Task -> a
  visitTitleTask :: v -> TitleTask -> a
  visitDescriptionTask :: v -> DescriptionTask -> a
  visitStateTask :: v -> StateTask -> a
  visitTagTask :: v -> TagTask -> a
  visitMembersTask :: v -> MembersTask -> a
  visitSubTasksTask :: v -> SubTasksTask -> a
  visitTakeTaskAttribute :: v -> TakeTaskAttribute -> a
  visitTakeTaskAttributeLiteral :: v -> TakeTaskAttributeLiteral -> a
  visitMember :: v -> Member -> a
  visitMemberName :: v -> MemberName -> a
  visitMemberRole :: v -> MemberRole -> a
  visitTakeMemberAttribute :: v -> TakeMemberAttribute -> a
  visitList :: v -> List -> a
  visitFunc :: v -> Func -> a
  visitFuncParam :: v -> FuncParam -> a
  visitFuncBody :: v -> FuncBody -> a
  visitPatternCase :: v -> PatternCase -> a
  visitPatternCaseValue :: v -> PatternCaseValue -> a
  visitPatternDefault :: v -> PatternDefault -> a
  visitFuncCall :: v -> FuncCall -> a
  visitFuncCallParam :: v -> FuncCallParam -> a
  visitBoolExpression :: v -> BoolExpression -> a
  visitBoolComparator :: v -> BoolComparator -> a
  visitComparison :: v -> Comparison -> a
  visitCondition :: v -> Condition -> a
  visitCycle :: v -> Cycle -> a
  visitCycleList :: v -> CycleList -> a
  visitStatement :: v -> Statement -> a
  visitDoNotation :: v -> DoNotation -> a
  visitDoStatement :: v -> DoStatement -> a
  visitPrint :: v -> Print -> a
  visitCode :: v -> Code -> a
