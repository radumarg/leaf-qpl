module Desugarer.DesugaringTest

import Test.Simple

import Desugarer.Helper

%default total

export
runDesugaringTests : IO ()
runDesugaringTests = runTests $ Test.do

  test "missing return type is filled in with -> ()" $
    desugarAndPrettyPrint "general fn empty() {}"
      `shouldBe` Just "general fn empty() -> () { }"

  test "missing effect is filled in with general" $
    desugarAndPrettyPrint "fn f() -> i32 { 1 }"
      `shouldBe` Just "general fn f() -> i32 { 1 }"

  test "default attribute argument is added if argument is missing" $
    desugarAndPrettyPrint "#[qasm_gate]\ngeneral fn myFun() -> () {}"
      `shouldBe` Just "#[qasm_gate(\"myFun\")]\ngeneral fn myFun() -> () { }"

  test "nested expressions round-trip through the canonical printer" $
    desugarAndPrettyPrint "fn add(x: i32) -> i32 { x + 1 }"
      `shouldBe` Just "general fn add(x: i32) -> i32 { (x + 1) }"

  test "missing classical else is filled in with a unit block" $
    desugarAndPrettyPrint "fn f() { if ready {} }"
      `shouldBe` Just "general fn f() -> () { if ready { } else { () } }"

  test "const declarations desugar unchanged" $
    desugarAndPrettyPrint "const N: i64 = 4;"
      `shouldBe` Just "const N: i64 = 4;"

  -- Parenthesization nodes carry no meaning past the surface AST
  -- so the canonical AST holds the bare inner node.

  test "ExprParenthesized is discarded during canonicalization (1)" $
    desugarAndPrettyPrint "fn wrap() -> i32 { ((1 + 2)) }"
      `shouldBe` Just "general fn wrap() -> i32 { (1 + 2) }"

  test "ExprParenthesized is discarded during canonicalization (2)" $
    desugarAndPrettyPrint "fn wrap() -> i32 { (3) }"
      `shouldBe` Just "general fn wrap() -> i32 { 3 }"

  test "TyParenthesized is discarded during canonicalization (3)" $
    desugarAndPrettyPrint "fn t(x: (i32)) -> (i32) { x }"
      `shouldBe` Just "general fn t(x: i32) -> i32 { x }"

  test "PatternParenthesized is discarded during canonicalization (4)" $
    desugarAndPrettyPrint "fn p() { let (a) = 1; }"
      `shouldBe` Just "general fn p() -> () { let a = 1; }"
