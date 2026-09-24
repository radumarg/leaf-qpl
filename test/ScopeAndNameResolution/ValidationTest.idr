module ScopeAndNameResolution.ValidationTest

import Compiler.ScopeAndNameResolution.Data
import Compiler.ScopeAndNameResolution.Validate
import Frontend.ASTData
import Frontend.ASTPhases
import Frontend.Source
import Frontend.Syntax.AST
import Frontend.Type

import Data.List
import Data.SnocList
import Data.SortedMap
import Test.Simple

%default total

info : Nat -> AstInfo
info id = MkAstInfo (MkNodeId id 0)
                   (MkSourceSpan "resolution-fixture.rs" (MkSourcePos 1 1) (MkSourcePos 1 2))

newScope : Nat -> ScopeKind -> Maybe ScopeId -> ScopeInfo
newScope id kind parent =
  MkScopeInfo (MkScopeId id) kind parent (SourceScope (info id)) empty empty empty [<]

binding : String -> Nat -> BindingKind -> ScopeBinding
binding name target kind =
  MkScopeBinding name (Just (info target).span) (MkSymbolId target) kind LexicalVisibility

rootSymbol : ResolvedSymbolInfo
rootSymbol = MkSymbolInfo (MkSymbolId 0) SymbolModule () "fixture"
                          (MkScopeId 0) (MkSymbolId 0) PublicVisibility (SourceSymbol (info 0))

localSymbol : ResolvedSymbolInfo
localSymbol = MkSymbolInfo (MkSymbolId 1) SymbolLocalBinding () "x"
                           (MkScopeId 1) (MkSymbolId 0) LexicalVisibility (SourceSymbol (info 1))

rootScope : ScopeInfo
rootScope = newScope 0 ModuleScope Nothing

blockScope : ScopeInfo
blockScope = { valueBindings := fromList [("x", binding "x" 1 DeclaredBinding)],
               declaredSymbolsInOrder := [< MkSymbolId 1] }
             (newScope 1 BlockScope (Just (MkScopeId 0)))

reference : SymbolReference
reference = MkSymbolReference (MkSymbolId 1) (info 10) (MkScopeId 1)
                              (MkNodeId 11 0) ValueReference "x"

-- Synthetic table fixtures: the validator checks stored relationships, not
-- completeness against an AST traversal. The source-file payload can be empty.
validModule : ResolvedModule
validModule = MkResolvedModule
  (MkSymbolId 0)
  (MkScopeId 0)
  (resolvedAstNode (info 0) WrittenCode (MkSourceFileNode [] []))
  (fromList [(MkNodeId 0 0, MkScopeId 0), (MkNodeId 10 0, MkScopeId 1)])
  (fromList [(MkSymbolId 0, rootSymbol), (MkSymbolId 1, localSymbol)])
  (fromList [(MkSymbolId 1, MkLocalVariableInfo ImmutableLocal NotQubitLocal)])
  (fromList [(MkSymbolId 1, [< reference])])
  (fromList [(MkScopeId 0, rootScope), (MkScopeId 1, blockScope)])
  (fromList [(MkSymbolId 0, MkScopeId 0)])

withSymbol : Nat -> ResolvedSymbolInfo -> ResolvedModule -> ResolvedModule
withSymbol key symbol resolved = { symbols := insert (MkSymbolId key) symbol resolved.symbols } resolved

withScope : Nat -> ScopeInfo -> ResolvedModule -> ResolvedModule
withScope key scope resolved = { scopes := insert (MkScopeId key) scope resolved.scopes } resolved

diagnostics : ResolvedModule -> List String
diagnostics = map show . validateResolvedModule

reports : ResolutionInvariantError -> ResolvedModule -> Bool
reports err resolved = elem (show err) (diagnostics resolved)

-- A private alias exposes a public function without changing its symbol identity
-- or adding that imported target to the block's declaration-order list.
aliasedModule : ResolvedModule
aliasedModule =
  let function = MkSymbolInfo (MkSymbolId 2) SymbolFunction () "calculate"
                             (MkScopeId 0) (MkSymbolId 0) PublicVisibility (SourceSymbol (info 2))
      declaringScope = { valueBindings := fromList [("calculate", binding "calculate" 2 DeclaredBinding)],
                         declaredSymbolsInOrder := [< MkSymbolId 2] } rootScope
      importingScope = { valueBindings := insert "calc" (binding "calc" 2 ImportedBinding) blockScope.valueBindings }
                       blockScope
      aliasReference = { target := MkSymbolId 2, writtenName := "calc", occurrence := info 12 } reference
      resolved = withSymbol 2 function $ withScope 0 declaringScope $ withScope 1 importingScope validModule
  in { references := insert (MkSymbolId 2) [< aliasReference] resolved.references } resolved

shadowedModule : ResolvedModule
shadowedModule =
  let laterSymbol = { symbolId := MkSymbolId 2, origin := SourceSymbol (info 2) } localSymbol
      laterScope = { valueBindings := fromList [("x", binding "x" 2 DeclaredBinding)],
                     declaredSymbolsInOrder := [< MkSymbolId 1, MkSymbolId 2] } blockScope
  -- The earlier reference to symbol 1 remains valid after symbol 2 shadows it.
  in withSymbol 2 laterSymbol $ withScope 1 laterScope validModule

memberModule : ResolvedModule
memberModule =
  let struct = MkSymbolInfo (MkSymbolId 2) SymbolStruct () "Point"
                           (MkScopeId 0) (MkSymbolId 0) PublicVisibility (SourceSymbol (info 2))
      field = MkSymbolInfo (MkSymbolId 3) SymbolField () "x"
                          (MkScopeId 2) (MkSymbolId 0) PublicVisibility (SourceSymbol (info 3))
      method = MkSymbolInfo (MkSymbolId 4) SymbolMethod () "update"
                           (MkScopeId 3) (MkSymbolId 0) PublicVisibility (SourceSymbol (info 4))
      declaringScope = { typeBindings := fromList [("Point", binding "Point" 2 DeclaredBinding)],
                         valueBindings := fromList [("Point", binding "Point" 2 DeclaredBinding)],
                         declaredSymbolsInOrder := [< MkSymbolId 2] } rootScope
      members = { fieldBindings := fromList [("x", binding "x" 3 DeclaredBinding)],
                  valueBindings := fromList [("update", binding "update" 4 DeclaredBinding)],
                  declaredSymbolsInOrder := [< MkSymbolId 3] }
                (newScope 2 MemberScope (Just (MkScopeId 0)))
      implScope = { valueBindings := fromList [("update", binding "update" 4 DeclaredBinding)],
                    declaredSymbolsInOrder := [< MkSymbolId 4] }
                  (newScope 3 ImplScope (Just (MkScopeId 0)))
      resolved = withSymbol 2 struct $ withSymbol 3 field $ withSymbol 4 method $
                 withScope 0 declaringScope $ withScope 2 members $ withScope 3 implScope validModule
  in { memberScopes := insert (MkSymbolId 2) (MkScopeId 2) resolved.memberScopes } resolved

export
runResolutionValidationTests : IO ()
runResolutionValidationTests = runTests $ Test.do

  test "consistent resolution tables and a reused root ModuleScope are accepted" $
    diagnostics validModule `shouldBe` []

  test "private import aliases preserve the public target's identity" $
    diagnostics aliasedModule `shouldBe` []

  test "prelude bindings need not appear in local declaration order" $
    diagnostics (withScope 1
      ({ valueBindings := insert "preludeAlias" (binding "preludeAlias" 0 PreludeBinding) blockScope.valueBindings }
       blockScope) validModule) `shouldBe` []

  test "shadowed declarations and earlier references remain valid" $
    diagnostics shadowedModule `shouldBe` []

  test "multiple namespaces share a symbol listed once in declaration order" $
    diagnostics memberModule `shouldBe` []

  test "impl methods may be exposed through a separate member scope" $
    diagnostics memberModule `shouldBe` []

  test "a module may also have a separate MemberScope" $
    diagnostics ({ memberScopes := fromList [(MkSymbolId 0, MkScopeId 2)] }
                 (withScope 2 (newScope 2 MemberScope Nothing) validModule)) `shouldBe` []

  test "ordinary parameters may have local-variable metadata" $
    diagnostics (withSymbol 1 ({ symbolKind := SymbolFunctionParameter } localSymbol) validModule) `shouldBe` []

  test "self receiver parameters may have local-variable metadata" $
    diagnostics (withSymbol 1 ({ symbolKind := SymbolSelfReceiverParameter } localSymbol) validModule) `shouldBe` []

  test "missing root modules are reported" $
    reports (MissingRootModule (MkSymbolId 99)) ({ rootModule := MkSymbolId 99 } validModule) `shouldBe` True

  test "the root module must be a module symbol" $
    diagnostics ({ rootModule := MkSymbolId 1 } validModule) `shouldBe`
      [show (InvalidRootModule (MkSymbolId 1))]

  test "missing root scopes are reported" $
    diagnostics ({ rootScope := MkScopeId 99 } validModule) `shouldBe`
      [show (MissingRootScope (MkScopeId 99))]

  test "the root scope must be a ModuleScope" $
    diagnostics ({ rootScope := MkScopeId 1 } validModule) `shouldBe`
      [show (InvalidRootScope (MkScopeId 1))]

  test "symbol table keys must agree with stored symbol IDs" $
    diagnostics (withSymbol 1 ({ symbolId := MkSymbolId 99 } localSymbol) validModule) `shouldBe`
      [show (SymbolKeyMismatch (MkSymbolId 1) (MkSymbolId 99))]

  test "symbol declaring scopes must exist" $
    reports (MissingDeclaringScope (MkSymbolId 1) (MkScopeId 99))
      (withSymbol 1 ({ declaringScope := MkScopeId 99 } localSymbol) validModule) `shouldBe` True

  test "symbol declaring modules must exist" $
    diagnostics (withSymbol 1 ({ declaringModule := MkSymbolId 99 } localSymbol) validModule) `shouldBe`
      [show (MissingDeclaringModule (MkSymbolId 1) (MkSymbolId 99))]

  test "symbol declaring modules must be module symbols" $
    diagnostics (withSymbol 1 ({ declaringModule := MkSymbolId 1 } localSymbol) validModule) `shouldBe`
      [show (InvalidDeclaringModule (MkSymbolId 1) (MkSymbolId 1))]

  test "scope table keys must agree with stored scope IDs" $
    diagnostics (withScope 1 ({ id := MkScopeId 99 } blockScope) validModule) `shouldBe`
      [show (ScopeKeyMismatch (MkScopeId 1) (MkScopeId 99))]

  test "scope parents must exist" $
    diagnostics (withScope 1 ({ parent := Just (MkScopeId 99) } blockScope) validModule) `shouldBe`
      [show (MissingParentScope (MkScopeId 1) (MkScopeId 99))]

  test "self-parent cycles terminate and are reported" $
    diagnostics (withScope 1 ({ parent := Just (MkScopeId 1) } blockScope) validModule) `shouldBe`
      [show (ScopeParentCycle (MkScopeId 1))]

  test "multi-scope cycles terminate and are reported once" $
    diagnostics (withScope 0 ({ parent := Just (MkScopeId 1) } rootScope) validModule) `shouldBe`
      [show (ScopeParentCycle (MkScopeId 0))]

  test "multiple paths into one cycle do not duplicate its diagnostic" $
    diagnostics (withScope 1 ({ parent := Just (MkScopeId 2) } blockScope) $
                 withScope 2 (newScope 2 BlockScope (Just (MkScopeId 1))) $
                 withScope 3 (newScope 3 BlockScope (Just (MkScopeId 1))) validModule) `shouldBe`
      [show (ScopeParentCycle (MkScopeId 1))]

  test "binding keys must agree with their written names" $
    diagnostics (withScope 1 ({ valueBindings := fromList [("wrong", binding "x" 1 DeclaredBinding)] }
                             blockScope) validModule) `shouldBe`
      [show (BindingNameMismatch (MkScopeId 1) ValueNamespace "wrong" "x")]

  test "type binding targets must exist" $
    diagnostics (withScope 1 ({ typeBindings := fromList [("T", binding "T" 99 ImportedBinding)] }
                             blockScope) validModule) `shouldBe`
      [show (MissingBindingTarget (MkScopeId 1) TypeNamespace "T" (MkSymbolId 99))]

  test "value binding targets must exist" $
    diagnostics (withScope 1 ({ valueBindings := fromList [("x", binding "x" 99 ImportedBinding)] }
                             blockScope) validModule) `shouldBe`
      [show (MissingBindingTarget (MkScopeId 1) ValueNamespace "x" (MkSymbolId 99))]

  test "field binding targets must exist" $
    diagnostics (withScope 1 ({ fieldBindings := fromList [("x", binding "x" 99 ImportedBinding)] }
                             blockScope) validModule) `shouldBe`
      [show (MissingBindingTarget (MkScopeId 1) FieldNamespace "x" (MkSymbolId 99))]

  test "declaration order must not contain missing symbols" $
    diagnostics (withScope 1 ({ declaredSymbolsInOrder := [< MkSymbolId 1, MkSymbolId 99] }
                             blockScope) validModule) `shouldBe`
      [show (MissingDeclaredSymbol (MkScopeId 1) (MkSymbolId 99))]

  test "declaration order must not contain duplicate symbol IDs" $
    diagnostics (withScope 1 ({ declaredSymbolsInOrder := [< MkSymbolId 1, MkSymbolId 1] }
                             blockScope) validModule) `shouldBe`
      [show (DuplicateDeclaredSymbol (MkScopeId 1) (MkSymbolId 1))]

  test "imported targets declared elsewhere are not local declarations" $
    reports (WrongDeclarationScope (MkScopeId 1) (MkSymbolId 2) (MkScopeId 0))
      (withScope 1 ({ declaredSymbolsInOrder := [< MkSymbolId 1, MkSymbolId 2] } blockScope) aliasedModule)
      `shouldBe` True

  test "locally declared bindings must appear in declaration order" $
    diagnostics (withScope 1 ({ declaredSymbolsInOrder := [<] } blockScope) validModule) `shouldBe`
      [show (MissingDeclarationOrderEntry (MkScopeId 1) (MkSymbolId 1))]

  test "even empty reference buckets must target existing symbols" $
    diagnostics ({ references := insert (MkSymbolId 99) [<] validModule.references } validModule) `shouldBe`
      [show (MissingReferenceSymbol (MkSymbolId 99))]

  test "reference targets must agree with their bucket keys" $
    diagnostics ({ references := fromList [(MkSymbolId 1, [< { target := MkSymbolId 0 } reference])] }
                 validModule) `shouldBe`
      [show (ReferenceKeyMismatch (MkSymbolId 1) (MkSymbolId 0) (MkNodeId 10 0))]

  test "a mismatched reference target is also checked for existence" $
    diagnostics ({ references := fromList [(MkSymbolId 1, [< { target := MkSymbolId 99 } reference])] }
                 validModule) `shouldBe`
      map show [ReferenceKeyMismatch (MkSymbolId 1) (MkSymbolId 99) (MkNodeId 10 0),
                MissingReferenceSymbol (MkSymbolId 99)]

  test "reference enclosing scopes must exist" $
    diagnostics ({ references := fromList [(MkSymbolId 1, [< { enclosingScope := MkScopeId 99 } reference])] }
                 validModule) `shouldBe`
      [show (MissingReferenceScope (MkNodeId 10 0) (MkScopeId 99))]

  test "node-scope mappings must target existing scopes" $
    diagnostics ({ nodeScopes := fromList [(MkNodeId 10 0, MkScopeId 99)] } validModule) `shouldBe`
      [show (MissingNodeScope (MkNodeId 10 0) (MkScopeId 99))]

  test "local-variable metadata must target existing symbols" $
    diagnostics ({ localVariables := fromList [(MkSymbolId 99, MkLocalVariableInfo ImmutableLocal NotQubitLocal)] }
                 validModule) `shouldBe`
      [show (MissingLocalSymbol (MkSymbolId 99))]

  test "local-variable metadata cannot describe modules" $
    diagnostics ({ localVariables := fromList [(MkSymbolId 0, MkLocalVariableInfo ImmutableLocal NotQubitLocal)] }
                 validModule) `shouldBe`
      [show (InvalidLocalSymbolKind (MkSymbolId 0))]

  test "an ordinary function symbol is not a runtime local" $
    diagnostics (withSymbol 1 ({ symbolKind := SymbolFunction } localSymbol) validModule) `shouldBe`
      [show (InvalidLocalSymbolKind (MkSymbolId 1))]

  test "member-scope owners must exist" $
    diagnostics ({ memberScopes := fromList [(MkSymbolId 99, MkScopeId 0)] } validModule) `shouldBe`
      [show (MissingMemberOwner (MkSymbolId 99))]

  test "local variables cannot own member namespaces" $
    reports (InvalidMemberOwner (MkSymbolId 1))
      ({ memberScopes := fromList [(MkSymbolId 1, MkScopeId 0)] } validModule) `shouldBe` True

  test "member-scope targets must exist" $
    diagnostics ({ memberScopes := fromList [(MkSymbolId 0, MkScopeId 99)] } validModule) `shouldBe`
      [show (MissingMemberScope (MkSymbolId 0) (MkScopeId 99))]

  test "member lookup cannot use a block scope" $
    diagnostics ({ memberScopes := fromList [(MkSymbolId 0, MkScopeId 1)] } validModule) `shouldBe`
      [show (InvalidMemberScopeKind (MkSymbolId 0) (MkScopeId 1))]

  test "a struct's members cannot use a module scope" $
    diagnostics ({ memberScopes := insert (MkSymbolId 2) (MkScopeId 0) memberModule.memberScopes }
                 memberModule) `shouldBe`
      [show (InvalidMemberScopeKind (MkSymbolId 2) (MkScopeId 0))]

  test "independent invariant failures are accumulated" $
    diagnostics ({ rootScope := MkScopeId 99,
                   nodeScopes := fromList [(MkNodeId 10 0, MkScopeId 99)] } validModule) `shouldBe`
      map show [MissingRootScope (MkScopeId 99), MissingNodeScope (MkNodeId 10 0) (MkScopeId 99)]
