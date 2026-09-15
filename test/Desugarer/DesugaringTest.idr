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

  test "explicitly typed qubit let binding is linear by default" $
    desugarAndPrettyPrint "fn allocate() { let q: qubit = qalloc(); }"
      `shouldBe`
      Just "general fn allocate() -> () { let linear q: qubit = qalloc(); }"

  test "explicitly typed qubit array let binding is linear by default" $
    desugarAndPrettyPrint "fn allocate() { let qs: [qubit; 3] = qalloc(3); }"
      `shouldBe`
      Just "general fn allocate() -> () { let linear qs: [qubit; 3] = qalloc(3); }"

  test "explicitly typed qubit tuple let binding is linear by default" $
    desugarAndPrettyPrint
      "fn allocate() { let qs: (qubit, qubit) = (qalloc(), qalloc()); }"
      `shouldBe`
      Just
        "general fn allocate() -> () { let linear qs: (qubit, qubit) = (qalloc(), qalloc()); }"

  test "mixed tuple containing a qubit is linear by default" $
    desugarAndPrettyPrint
      "fn allocate() { let pair: (i32, qubit) = (0, qalloc()); }"
      `shouldBe`
      Just
        "general fn allocate() -> () { let linear pair: (i32, qubit) = (0, qalloc()); }"

  test "parenthesized qubit type is linear by default" $
    desugarAndPrettyPrint "fn allocate() { let q: ((qubit)) = qalloc(); }"
      `shouldBe`
      Just "general fn allocate() -> () { let linear q: qubit = qalloc(); }"

  test "qubit reference let binding is not linear by default" $
    desugarAndPrettyPrint "fn borrow(q: &qubit) { let qref: &qubit = &q; }"
      `shouldBe`
      Just
        "general fn borrow(q: &qubit) -> () { let qref: &qubit = (&q); }"

  test "classical array let binding is not linear by default" $
    desugarAndPrettyPrint
      "fn values() { let values: [i32; 3] = [0, 0, 0]; }"
      `shouldBe`
      Just
        "general fn values() -> () { let values: [i32; 3] = [0, 0, 0]; }"

  test "explicit affine qualifier is preserved for a qubit let binding" $
    desugarAndPrettyPrint "fn allocate() { let affine q: qubit = qalloc(); }"
      `shouldBe`
      Just "general fn allocate() -> () { let affine q: qubit = qalloc(); }"

  test "scratch qubit let binding is linear by default" $
    desugarAndPrettyPrint "fn allocate() { let scratch q: qubit = qalloc(); }"
      `shouldBe`
      Just
        "general fn allocate() -> () { let scratch linear q: qubit = qalloc(); }"

  test "inferred qubit let binding receives no qualifier during desugaring" $
    desugarAndPrettyPrint "fn allocate() { let q = qalloc(); }"
      `shouldBe`
      Just "general fn allocate() -> () { let q = qalloc(); }"

  test "default attribute argument is added if argument is missing" $
    desugarAndPrettyPrint "#[qasm_gate]\ngeneral fn myFun() -> () {}"
      `shouldBe` Just "#[qasm_gate(\"myFun\")]\ngeneral fn myFun() -> () { }"

  test "nested expressions round-trip through the canonical printer" $
    desugarAndPrettyPrint "fn add(x: i32) -> i32 { x + 1 }"
      `shouldBe` Just "general fn add(x: i32) -> i32 { (x + 1) }"

  test "+= becomes assignment with addition" $
    desugarAndPrettyPrint "fn assign() { x += 1; }"
      `shouldBe` Just "general fn assign() -> () { x = (x + 1); }"

  test "-= becomes assignment with subtraction" $
    desugarAndPrettyPrint "fn assign() { x -= 1; }"
      `shouldBe` Just "general fn assign() -> () { x = (x - 1); }"

  test "*= becomes assignment with multiplication" $
    desugarAndPrettyPrint "fn assign() { x *= 2; }"
      `shouldBe` Just "general fn assign() -> () { x = (x * 2); }"

  test "/= becomes assignment with division" $
    desugarAndPrettyPrint "fn assign() { x /= 2; }"
      `shouldBe` Just "general fn assign() -> () { x = (x / 2); }"

  test "%= becomes assignment with remainder" $
    desugarAndPrettyPrint "fn assign() { x %= 2; }"
      `shouldBe` Just "general fn assign() -> () { x = (x % 2); }"

  test "&= becomes assignment with bitwise and" $
    desugarAndPrettyPrint "fn assign() { x &= 3; }"
      `shouldBe` Just "general fn assign() -> () { x = (x & 3); }"

  test "|= becomes assignment with bitwise or" $
    desugarAndPrettyPrint "fn assign() { x |= 3; }"
      `shouldBe` Just "general fn assign() -> () { x = (x | 3); }"

  test "^= becomes assignment with bitwise xor" $
    desugarAndPrettyPrint "fn assign() { x ^= 3; }"
      `shouldBe` Just "general fn assign() -> () { x = (x ^ 3); }"

  test "<<= becomes assignment with shift left" $
    desugarAndPrettyPrint "fn assign() { x <<= 2; }"
      `shouldBe` Just "general fn assign() -> () { x = (x << 2); }"

  test ">>= becomes assignment with shift right" $
    desugarAndPrettyPrint "fn assign() { x >>= 2; }"
      `shouldBe` Just "general fn assign() -> () { x = (x >> 2); }"

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
