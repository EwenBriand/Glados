module TestValidState
  ( testReturn,
    testBind,
    testFunctor,
    testApplicative,
    testEq,
    testMonad,
    testFromValidState,
    testShowAndOrd,
  )
where

import EvaluateAST
import Instructions
import Lexer
import Test.HUnit
import VM
import ValidState

returnTest :: ValidState Int
returnTest = do
  let value = 24
  ValidState.return value

testReturn :: Test
testReturn =
  TestList
    ["return test" ~: returnTest ~?= Valid 24]

bindTest :: ValidState Int -> ValidState Int
bindTest m = do
  let f x = Valid (x + 10)
  let res = m ValidState.>>= f
  res

testBind :: Test
testBind =
  TestList
    [ "bind with Valid value"
        ~: bindTest (Valid 42) ~?= Valid 52,
      "bind with Invalid value"
        ~: bindTest (Invalid "bad everything") ~?= Invalid "bad everything",
        ">>= should work for Invalid" ~: do
        let action = Invalid "error message" ValidState.>>= (\x -> ValidState.return (x * 2))
        assertEqual "Invalid 'error message' >>= (x -> return (x * 2))" (Invalid "error message") action
    ]

testFunctor :: Test
testFunctor =
  TestList
    [ "maps over a Valid value"
        ~: let m = Valid 42
            in fmap (+ 10) m ~?= Valid 52,
      "maps over an Invalid value"
        ~: let m = Invalid "error message"
            in fmap (+ 10) m ~?= Invalid "error message"
    ]

testApplicative :: Test
testApplicative =
  TestList
    [ "applies a Valid function to a Valid value"
        ~: let f = Valid ((+ 10) :: Int -> Int)
               x = Valid (42 :: Int)
            in f <*> x ~?= (Valid 52 :: ValidState Int),
      "applies an Invalid function to a Valid value"
        ~: let f = Invalid "error message"
               x = Valid (42 :: Int)
            in f <*> x ~?= (Invalid "error message" :: ValidState String),
      "applies a Valid function to an Invalid value"
        ~: let f = Valid ((+ 10) :: Int -> Int)
               x = Invalid "error message"
            in f <*> x ~?= Invalid "error message",
      "applies an Invalid function to an Invalid value"
        ~: let f = Invalid "error message 1"
               x = Invalid "error message 2"
            in f <*> x ~?= (Invalid "error message 1" :: ValidState String),
      "creates a Valid value"
        ~: let x = 42
            in pure x ~?= Valid x
    ]

testEq :: Test
testEq =
  TestList
    [ "compares Valid values for equality"
        ~: (Valid 42 :: ValidState Int) == (Valid 42 :: ValidState Int) ~?= True,
      "compares Invalid values for equality"
        ~: (Invalid "Error" :: ValidState String) == (Invalid "Error" :: ValidState String) ~?= True,
      "compares Invalid values with different messages"
        ~: (Invalid "Error1" :: ValidState String) == (Invalid "Error2" :: ValidState String) ~?= False,
      "compares Invalid values with TestError message"
        ~: (Invalid "Error" :: ValidState String) == (Invalid "TestError" :: ValidState String) ~?= True,
      "compares Valid and Invalid values"
        ~: (Valid 42 :: ValidState Int) == Invalid "Error" ~?= False,
      "compares Invalid and Valid values"
        ~: Invalid "Error" == (Valid 42 :: ValidState Int) ~?= False
    ]

testBindInvalid :: Test
testBindInvalid = ">>= should work for Invalid ValidState" ~: do
    let action = Invalid "error message" ValidState.>>= (\x -> ValidState.return (x * 2))
    assertEqual "Invalid 'error message' >>= (x -> return (x * 2))" (Invalid "error message") action

testMonad :: Test
testMonad =
  TestList
    [ "returns a ValidState monadic value with the given value"
        ~: let x = 42
            in ValidState.return x ~?= Valid x,
      "applies the function to the value inside the ValidState monadic value"
        ~: let f = \x -> Valid (x * 2)
            in (Valid 42 ValidState.>>= f) ~?= Valid 84,
      "returns the same Invalid value without applying the function"
        ~: let f = \x -> Valid (x * 2)
            in (Invalid "Error" ValidState.>>= f) ~?= Invalid "Error",
      "discards the result of the function and returns the same Invalid value"
        ~: let f = \_ -> Invalid "Error"
            in (Valid 42 ValidState.>>= f) ~?= (Invalid "Error" :: ValidState String),
      "returns the same Invalid value without applying the function"
        ~: let f = \x -> Valid (x * 2)
            in (Invalid "Error" ValidState.>>= f) ~?= Invalid "Error",
      "discards the result of the function and returns the same Invalid value"
        ~: let f = \_ -> Invalid "Error"
            in (Invalid "Error" ValidState.>>= f) ~?= (Invalid "Error" :: ValidState String),
      "bind Invalid value with a function"
        ~: do
          let value = Invalid "Error"
              result = value ValidState.>>= (\x -> Valid (x + 10))
          assertEqual "Result should be Invalid with the same message" (Invalid "Error") result,
      testBindInvalid
    ]

testFromValidState :: Test
testFromValidState =
  TestList
    [ "returns the value inside a ValidState monadic value"
        ~: let x = 42
            in fromValidState 0 (Valid x) ~?= x,
      "returns the default value for an Invalid value"
        ~: let d = "Error"
            in fromValidState d (Invalid "Error") ~?= d
    ]

testShowAndOrd :: Test
testShowAndOrd =
  TestList
    [ "returns a string representation of a ValidState monadic value"
        ~: let x = 42 :: Int
            in show (Valid x) ~?= "Valid " ++ show x,
      "returns a string representation of an Invalid value"
        ~: show (Invalid "Error" :: ValidState String) ~?= "Invalid \"Error\"",
      "compares two ValidState monadic values based on their inner values"
        ~: let x = Valid 42
               y = Valid 84
               z = Invalid "Error"
            in [x, y, z] ~?= [x, y, z],
      "compares two ValidState monadic values based on their inner values 2"
        ~: let x = Invalid "foo"
               y = Invalid "bar"
               z = Valid 42
            in [x, y, z] `compare` [y, x, z] ~?= GT,
      "compares two ValidState monadic values based on their inner values"
        ~: let x = Valid 42
               y = Valid 84
               z = Invalid "Error"
            in [x, y, z] `compare` [x, z, y] ~?= LT,
      "compares two ValidState monadic values based on their inner values"
        ~: let x = Valid "foo"
               y = Valid "bar"
               z = Invalid "Error"
            in [x, y, z] `compare` [x, z, y] ~?= LT,
      "compares two ValidState monadic values based on their inner values"
        ~: let x = Valid 42
               y = Valid 84
               z = Invalid "Error"
            in [x, y, z] `compare` [y, x, z] ~?= LT,
      "compares two ValidState monadic values based on their inner values"
        ~: let x = Valid "foo"
               y = Valid "bar"
               z = Invalid "Error"
            in [x, y, z] `compare` [y, x, z] ~?= GT,
      "compares two ValidState monadic values based on their inner values"
        ~: let x = Invalid "foo"
               y = Invalid "bar"
               z = Valid 42
            in [x, y, z] `compare` [y, x, z] ~?= GT,
      "compares two ValidState monadic values based on their inner values"
        ~: let x = Valid 42
               y = Valid 84
               z = Invalid "Error"
            in [x] >= [y] ~?= False,
      "compares two ValidState monadic values based on their inner values"
        ~: let x = Valid 42
               y = Valid 84
               z = Invalid "Error"
            in [x] <= [y] ~?= True,
      "compares two ValidState monadic values based on their inner values"
        ~: let x = Valid 42
               y = Valid 42
               z = Invalid "Error"
            in [x] <= [y] ~?= True,
      "compares two ValidState monadic values based on their inner values"
        ~: let x = Valid 42
               y = Valid 42
               z = Invalid "Error"
            in [x] >= [y] ~?= True,
      "compares two ValidState monadic values based on their inner values"
        ~: let x = Valid 42
               y = Valid 42
               z = Invalid "Error"
            in [x] == [y] ~?= True,
      "compares two ValidState monadic values based on their inner values"
        ~: let x = Valid 84
               y = Valid 42
               z = Invalid "Error"
            in [x] > [y] ~?= True,
      "compares two ValidState monadic values based on their inner values"
        ~: let x = Valid 42
               y = Valid 84
               z = Invalid "Error"
            in [x] < [y] ~?= True,
      "compares two ValidState monadic values based on their inner values"
        ~: let x = Valid 42
               y = Valid 84
               z = Invalid "Error"
            in [x] > [y] ~?= False,
      "compares two ValidState monadic values based on their inner values"
        ~: let x = Valid 42
               y = Valid 84
               z = Invalid "Error"
            in [y] < [x] ~?= False,
      "compares two ValidState monadic values based on their inner values"
        ~: let x = Valid 42
               y = Valid 84
               z = Invalid "Error"
            in [y] < [z] ~?= True,
      "compares two ValidState monadic values based on their inner values"
        ~: let x = Valid 42
               y = Valid 84
               z = Invalid "Error"
            in [x] > [z] ~?= False,
      "compares two ValidState monadic values based on their inner values"
        ~: let x = Valid 42
               y = Valid 84
               z = Invalid "Error"
            in [x] == [z] ~?= False,
      "compares two ValidState monadic values based on their inner values"
        ~: let x = Valid 42
               y = Valid 84
               z = Invalid "Error"
            in [x] == [y] ~?= False
    ]
