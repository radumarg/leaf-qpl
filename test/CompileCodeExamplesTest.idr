module CompileCodeExamplesTest

import Test.Simple

import CompileCodeExamples

%default total

isFailure : Either String () -> Bool
isFailure (Left _)  = True
isFailure (Right _) = False

export
runCompileCodeExamplesTests : IO ()
runCompileCodeExamplesTests = runTests $ Test.do

  test "the source compilation pipeline accepts a valid program" $
    compileLeafSource "test-fixture.rs" "fn f() {}" `shouldBe` Right ()

  test "the source compilation pipeline reports lexer failures" $
    isFailure (compileLeafSource "test-fixture.rs" "@") `shouldBe` True

  test "the source compilation pipeline reports parser failures" $
    isFailure (compileLeafSource "test-fixture.rs" "fn f( {}") `shouldBe` True

  test "the source compilation pipeline reports every validation failure" $
    compileLeafSource "test-fixture.rs" "fn f() {break; continue;}" `shouldBe`
      Left
        ( "Validation errors in test-fixture.rs:\n" ++
          "test-fixture.rs:1:9: `break` found outside of a loop\n" ++
          "test-fixture.rs:1:16: `continue` found outside of a loop\n"
        )
