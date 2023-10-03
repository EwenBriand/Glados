import Test.HUnit

import TestTokenizer
import TestLexer
import TestEvaluateAST
import TestInstructions
import TestVM


main :: IO()
main = do
  _ <- runTestTT testTryTokenizeOne
  _ <- runTestTT testTokenInfoFields
  _ <- runTestTT testTokenInfoShow
  _ <- runTestTT testWordToToken
  _ <- runTestTT testTokenize
  _ <- runTestTT testASTNodeFields
  _ <- runTestTT testShowASTNode
  _ <- runTestTT testShowVarType
  _ <- runTestTT testShowTokorNode
  _ <- runTestTT testTokOrExprToNode
  _ <- runTestTT testTryToMatch
  _ <- runTestTT testBuildASTIterate
  _ <- runTestTT testBuildAST
  _ <- runTestTT testTryBuildInstructionList
  _ <- runTestTT testStrToAST
  _ <- runTestTT testIncRegister
  _ <- runTestTT testIncRegisterInvalidContext
  _ <- runTestTT testDecRegister
  _ <- runTestTT testDecRegisterInvalidContext
  _ <- runTestTT testAddRegister
  _ <- runTestTT testAddRegisterInvalidContext
  _ <- runTestTT testSubRegister
  _ <- runTestTT testSubRegisterInvalidContext
  _ <- runTestTT testMulRegister
  _ <- runTestTT testMulRegisterInvalidContext
  _ <- runTestTT testDivRegister
  _ <- runTestTT testDivZeroRegister
  _ <- runTestTT testDivRegisterInvalidContext
  _ <- runTestTT testModRegister
  _ <- runTestTT testModZeroRegister
  _ <- runTestTT testModRegisterInvalidContext
  _ <- runTestTT testAndRegister
  _ <- runTestTT testAndRegisterInvalidContext
  _ <- runTestTT testOrRegister
  _ <- runTestTT testOrRegisterInvalidContext
  _ <- runTestTT testXorRegister
  _ <- runTestTT testXorRegisterInvalidContext
  _ <- runTestTT testNotRegister
  _ <- runTestTT testNotRegisterInvalidContext
  _ <- runTestTT testStackPushPopPeek
  _ <- runTestTT testStackPushPop
  _ <- runTestTT testStackDup
  _ <- runTestTT testStackSwap
  _ <- runTestTT testStackRot
  _ <- runTestTT testHeapAlloc
  _ <- runTestTT testHeapAllocBis
  _ <- runTestTT testHeapSetGet
  _ <- runTestTT testLabelSetGet
  _ <- runTestTT testFlagGetSet
  _ <- runTestTT testInstructionFromAST
  _ <- runTestTT testMov
  _ <- runTestTT testCmp
  _ <- runTestTT testInc
  _ <- runTestTT testJmp
  _ <- runTestTT testAdd
  _ <- runTestTT testSub
  _ <- runTestTT testMult
  _ <- runTestTT testDiv
  _ <- runTestTT testMod
  _ <- runTestTT testXor
  _ <- runTestTT testAnd
  _ <- runTestTT testOr
  _ <- runTestTT testNot
  _ <- runTestTT testPushInstr
  _ <- runTestTT testAstToInstr
  _ <- runTestTT testArrToHASM
  _ <- runTestTT testStrToHASM
  _ <- runTestTT testInstructionList
  _ <- runTestTT testMovStackAddr
  _ <- runTestTT testputDefineInstruction
  _ <- runTestTT testMovFromStackAddr
  _ <- runTestTT testInstructionFromAST
  _ <- runTestTT testIf
  return ()
