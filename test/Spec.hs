{-# LANGUAGE BlockArguments #-}

import System.Exit
import System.Exit (ExitCode (..), exitWith)
import Test.HUnit
import Test.HUnit.Text (runTestTT)
import TestEvaluateAST
import TestInstructions
import TestLexer
import TestRealASM
import TestTokenizer
import TestVM
import TestVM (testBlockShow)
import TestValidState

-- Define a list of test functions
testFunctions :: [Test]
testFunctions =
  [ testTokenize,
    testTryTokenizeOne,
    testTokenInfoFields,
    testTokenInfoShow,
    testWordToToken,
    testASTNodeFields,
    testShowASTNode,
    testShowVarType,
    testShowTokorNode,
    testTokOrExprToNode,
    testTryToMatch,
    testBuildASTIterate,
    testBuildAST,
    testTryBuildInstructionList,
    testStrToAST,
    testIncRegister,
    testIncRegisterInvalidContext,
    testDecRegister,
    testDecRegisterInvalidContext,
    testAddRegister,
    testAddRegisterInvalidContext,
    testSubRegister,
    testSubRegisterInvalidContext,
    testMulRegister,
    testMulRegisterInvalidContext,
    testDivRegister,
    testDivZeroRegister,
    testDivRegisterInvalidContext,
    testModRegister,
    testModZeroRegister,
    testModRegisterInvalidContext,
    testAndRegister,
    testAndRegisterInvalidContext,
    testOrRegister,
    testOrRegisterInvalidContext,
    testXorRegister,
    testXorRegisterInvalidContext,
    testNotRegister,
    testNotRegisterInvalidContext,
    testStackPushPopPeek,
    testStackPushPop,
    testStackDup,
    testStackSwap,
    testStackRot,
    testHeapAlloc,
    testHeapAllocBis,
    testHeapSetGet,
    testExecSyscallWrapper,
    testLabelSetGet,
    testFlagGetSet,
    testInstructionFromAST,
    testMov,
    testCmp,
    testInc,
    testJmp,
    testAdd,
    testSub,
    testMult,
    testDiv,
    testMod,
    testXor,
    testAnd,
    testOr,
    testNot,
    testPushInstr,
    testAstToInstr {- testArrToHASM,-} {- testStrToHASM, -},
    testInstructionList,
    testMovStackAddr,
    testputDefineInstruction,
    testMovFromStackAddr,
    testInstructionFromAST,
    testIf,
    testPutDefineInstruction,
    testFuncCall,
    testReturn,
    testBind,
    testFunctor,
    testApplicative,
    TestValidState.testEq,
    testMonad,
    testFromValidState,
    testShowAndOrd,
    testCodeFromValidStateInt,
    testExecSyscall,
    testCallEasyPrint,
    testCodeFromEAX,
    testBlock,
    testBlockMap,
    testBlockShow,
    testBlockAddInvalid,
    testSymTable,
    testHeap,
    testStackPush,
    testStackPop,
    testStackPeek,
    testStackDup2,
    testStackSwap2,
    testStackRot2,
    testStackGetPointer,
    testParam,
    testInstruction,
    testGetTrueValueFromParam,
    testSetTrueValueFromParam,
    TestVM.testEq,
    testShow,
    testOrd,
    testInstructionTable,
    testPutSymbolInstruction,
    testRegInvalids,
    testAllocHeap,
    testInstructionTableIO,
    testEvalOneInstructionIO,
    testAllTest,
    testMyTest,
    testStackClear,
    testEncodeMov,
    testStackGetValueFromIndex,
    testAddressDoesntExist,
    testEncodeMovqRegImm,
    testInvalidHeap,
    testSymGet,
    testAdaptValueToVarType,
    testSysPrintValue,
    testsLabelsFuncs,
    testFlagInvalid,
    testContext,
    testNextUUIDValid,
    testIps,
    testHasmNStackPush,
    testLabels,
    testSetupfunctionStack,
    testParamsRegister,
    testInvalidFuncs,
    testInferTypeFromNode,
    testPutSetNoErrCheck,
    testArraysFuncs,
    testExecInstructionsIO,
    testInvalidInstructions
  ]

-- Run all test functions and return the results as a list
runTests :: IO [Counts]
runTests = sequence [runTestTT test | test <- testFunctions]

main :: IO ()
main = do
  putStrLn "Tests are starting..."
  results <- runTests
  if any (\r -> errors r > 0 || failures r > 0) results
    then
      ( do
          putStrLn "There are errors."
          exitWith (ExitFailure 1)
      )
    else putStrLn "All tests passed."
