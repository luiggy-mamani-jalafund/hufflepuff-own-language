module SemanticAnalizer where

import AbstractSyntaxTree


verifyCode :: Code -> Bool
verifyCode (Code funcs (DoStatement doStmt)) =
  all verifyFunction funcs && verifyFuncCall doStmt


verifyLiteralType :: Literal -> Type -> Bool
verifyLiteralType (LStringId _) TStringId = True
verifyLiteralType (LStringIdSpaces _) TStringIdSpace = True
verifyLiteralType (LStringParagraph _) TStringParagraph = True
verifyLiteralType (LTTAStrings _) TStringId = True
verifyLiteralType (LTMAStrings _) TStringId = True
verifyLiteralType _ _ = False

verifyValueType :: Value -> Type -> Bool
verifyValueType (ValLiteral lit) t = verifyLiteralType lit t
verifyValueType (ValTask _) TTask = True
verifyValueType (ValTag _) TTag = True
verifyValueType (ValMember _) TMember = True
verifyValueType (ValList lst) t = verifyListType lst t
verifyValueType (ValBool _) TBool = True
verifyValueType _ _ = False

verifyListType :: List -> Type -> Bool
verifyListType (ListStringId _) TListStringId = True
verifyListType (ListStringIdSpace _) TListStringIdSpace = True
verifyListType (ListStringParagraph _) TListStringParagraph = True
verifyListType (ListBool _) TListBool = True
verifyListType (ListTask _) TListTask = True
verifyListType (ListTag _) TListTag = True
verifyListType (ListState _) TListState = True
verifyListType (ListMember _) TListMember = True
verifyListType (ListList _) TListList = True
verifyListType _ _ = False

verifyFunction :: Func -> Bool
verifyFunction (Func _ returnType params body) =
  all verifyParam params && verifyFuncBody returnType body
  where
    verifyParam (FunParam _ t) = True
    verifyFuncBody retType (FuncReturn stmt) = verifyStatement stmt retType
    verifyFuncBody retType (FuncPattern cases defaultCase) =
      all (verifyPatternCase retType) cases && verifyPatternDefault retType defaultCase

    verifyPatternCase retType (Case values stmt) =
      all verifyPatternCaseValue values && verifyStatement stmt retType

    verifyPatternCaseValue (PCaseValue val) = True
    verifyPatternCaseValue PCaseEmpty = True

    verifyPatternDefault retType (PDefault stmt) = verifyStatement stmt retType


verifyFuncCall :: FuncCall -> Bool
verifyFuncCall (FuncCall _ params) = all verifyFuncCallParam params
  where
    verifyFuncCallParam (FCParamValue val) = True
    verifyFuncCallParam (FCParam call) = verifyFuncCall call

verifyBoolExpression :: BoolExpression -> Bool
verifyBoolExpression (BExp _) = True
verifyBoolExpression (BComparison comp) = verifyComparison comp
  where
    verifyComparison (CString lit1 _ lit2) = verifyLiteralType lit1 TStringId && verifyLiteralType lit2 TStringId
    verifyComparison (CBool _ _ _) = True
    verifyComparison (CTask _ _ _) = True
    verifyComparison (CMember _ _ _) = True


verifyStatement :: Statement -> Type -> Bool
verifyStatement (SFuncCall call) _ = verifyFuncCall call
verifyStatement (SValue val) t = verifyValueType val t
verifyStatement (STTA _) _ = True
verifyStatement (STMA _) _ = True
verifyStatement (SBoolExp exp) _ = verifyBoolExpression exp
verifyStatement (SBoolCondition cond) _ = verifyCondition cond
verifyStatement (SCycle cycle) _ = verifyCycle cycle

verifyCondition (Condition ifExp thenStmt elseStmt) =
  verifyBoolExpression ifExp && verifyStatement thenStmt TBool && verifyStatement elseStmt TBool

verifyCycle (Cycle mapRef mapList) = verifyCycleList mapList
  where
    verifyCycleList (CycleList lst) = verifyListType lst TListTask
    verifyCycleList (CycleId _) = True
