{-# LANGUAGE OverloadedStrings #-}

module MemberSpec (spec) where

import Test.Hspec
import Text.Parsec (parse)
import HpParser (member)
import AbstractSyntaxTree
import SymbolTable (emptyTable, insertMember, lookupSymbol, removeSymbol, SymbolInfo(..))
import Lexer (whiteSpace)

spec :: Spec
spec = do
  describe "Member Parser" $ do

    it "parses Member with MemberValueName and MemberValueRole" $ do
      let input = "Member { name: \"John Doe\", role: \"Developer\" }"
          result = parse (whiteSpace *> member emptyTable) "" input
          expectedMember = Member { name = MemberValueName (String "John Doe"), role = MemberValueRole (StringId "Developer") }
      case result of
        Right (member', _) -> member' `shouldBe` expectedMember
        _                  -> expectationFailure "Parsing failed"

    it "parses Member with MemberIdentifierName and MemberIdsentifierRole" $ do
      let input = "Member { name: nameId, role: roleId }"
          result = parse (whiteSpace *> member emptyTable) "" input
          expectedMember = Member { name = MemberIdentifierName "nameId", role = MemberIdentifierRole "roleId" }
      case result of
        Right (member', _) -> member' `shouldBe` expectedMember
        _                  -> expectationFailure "Parsing failed"

    it "parses Member with MemberTakeName and MemberTakeRole" $ do
      let input = "Member { name: takeName.name, role: takeRole.role }"
          result = parse (whiteSpace *> member emptyTable) "" input
          expectedMember = Member { name = MemberTakeName "takeName", role = MemberTakeRole "takeRole" }
      case result of
        Right (member', _) -> member' `shouldBe` expectedMember
        _                  -> expectationFailure "Parsing failed"

    it "parses NoAssigned member" $ do
      let input = "NoAssigned"
          result = parse (whiteSpace *> member emptyTable) "" input
          expectedMember = NoAssigned
      case result of
        Right (member', _) -> member' `shouldBe` expectedMember
        _                  -> expectationFailure "Parsing failed"

  describe "SymbolTable" $ do

    it "inserts and looks up a member" $ do
      let member = Member { name = MemberValueName (String "John Doe"), role = MemberValueRole (StringId "Developer") }
          symTable = insertMember "john_doe" member emptyTable
      lookupSymbol "john_doe" symTable `shouldBe` Just (MemberInfo "john_doe" member)

    it "removes a member" $ do
      let member = Member { name = MemberValueName (String "John Doe"), role = MemberValueRole (StringId "Developer") }
          symTable = insertMember "john_doe" member emptyTable
          symTable' = removeSymbol "john_doe" symTable
      lookupSymbol "john_doe" symTable' `shouldBe` Nothing

    it "inserts and looks up multiple members" $ do
      let member1 = Member { name = MemberValueName (String "John Doe"), role = MemberValueRole (StringId "Developer") }
          member2 = Member { name = MemberValueName (String "Jane Smith"), role = MemberValueRole (StringId "Manager") }
          member3 = Member { name = MemberValueName (String "Alice Brown"), role = MemberValueRole (StringId "Designer") }
          member4 = Member { name = MemberValueName (String "Bob White"), role = MemberValueRole (StringId "Tester") }
          member5 = Member { name = MemberValueName (String "Eve Black"), role = MemberValueRole (StringId "DevOps") }
          symTable = insertMember "john_doe" member1 $
                     insertMember "jane_smith" member2 $
                     insertMember "alice_brown" member3 $
                     insertMember "bob_white" member4 $
                     insertMember "eve_black" member5 emptyTable
      lookupSymbol "john_doe" symTable `shouldBe` Just (MemberInfo "john_doe" member1)
      lookupSymbol "jane_smith" symTable `shouldBe` Just (MemberInfo "jane_smith" member2)
      lookupSymbol "alice_brown" symTable `shouldBe` Just (MemberInfo "alice_brown" member3)
      lookupSymbol "bob_white" symTable `shouldBe` Just (MemberInfo "bob_white" member4)
      lookupSymbol "eve_black" symTable `shouldBe` Just (MemberInfo "eve_black" member5)

