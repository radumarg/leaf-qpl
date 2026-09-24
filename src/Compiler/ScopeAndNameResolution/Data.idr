module Compiler.ScopeAndNameResolution.Data

import Frontend.ASTData
import Frontend.ASTPhases
import Frontend.Source
import Frontend.Syntax.AST
import Frontend.Type

import Data.SortedMap

%default total

public export
ResolvedSymbolInfo : Type
ResolvedSymbolInfo = SymbolInfo ()

public export
data LocalMutability
  = ImmutableLocal
  | MutableLocal

public export
data QubitStorageStatus
  = NotQubitLocal
  | PendingQubitDefault
  | AssignedQubitStorage QuantumStorageProperties

public export
record LocalVariableInfo where
  constructor MkLocalVariableInfo
  mutability     : LocalMutability
  quantumStorage : QubitStorageStatus

public export
record ResolvedModule where
  constructor MkResolvedModule
  rootModule     : SymbolId
  rootScope      : ScopeId
  ast            : ResolvedSourceFile
  nodeScopes     : SortedMap NodeId ScopeId                 -- lexical environment used to resolve a node
  symbols        : SortedMap SymbolId ResolvedSymbolInfo
  localVariables : SortedMap SymbolId LocalVariableInfo     -- Local/parameter declaration metadata, keyed by the corresponding SymbolId.
  references     : SortedMap SymbolId (SnocList SymbolReference)
  scopes         : SortedMap ScopeId ScopeInfo
  memberScopes   : SortedMap SymbolId ScopeId               -- I have Point’s SymbolId. Which ScopeId contains its members?


public export
data ResolutionError
  = UnresolvedName String SourceSpan                         -- No matching declaration is visible at the use site
  | DuplicateDeclaration String (List SymbolId) SourceSpan   -- Declarations conflict in the same scope and namespace,
                                                             -- example: two functions named `helper` in one module.
  | InaccessibleSymbol SymbolId SourceSpan                   -- Declaration exists but visibility rules prohibit access,
                                                             -- example: accessing a private function from a sibling module.

-- DuplicatePatternBinding String SourceSpan SourceSpan     -- Pattern introduces the same variable twice: `let (x, x) = pair;`.
                                                             -- Covered by PostParseValidation, should not occur.
-- IllegalCapture String SourceSpan                         -- Nested functions are forbidden from capturing enclosing locals,
                                                             -- example: `fn outer() { let x = 1; fn inner() -> i32 { x } }`
-- IllegalShadowing String SymbolId SourceSpan SourceSpan   -- A declaration shadows a protected name,
-- AmbiguousName String (List SymbolId) SourceSpan          -- Lookup finds multiple candidates without a rule selecting one.
-- UnresolvedPathSegment, when a component of a qualified path cannot be found, example: `my_library::missing::helper`.
-- InvalidPathQualifier, when a resolved symbol cannot qualify another path component, example: `count::helper` where `count` is an integer variable.
-- ConflictingImports, when imports introduce different symbols under the same name where forbidden, example: `use a::helper; use b::helper;`.
-- WrongNamespace, when a name exists only in a namespace incompatible with its use, example: `let y: x = 0;` where `x` names only a variable.
-- UseBeforeDeclaration, optionally when a local variable is referenced before its declaration, example: `let y = x; let x = 1;` with no earlier `x` in scope.
-- ModuleNotFound, during module loading when an external module cannot be located, example: `mod my_library;` with no corresponding module source.
-- CyclicImport, when an import cycle prevents resolution or violates Leaf’s rules, example: `a::helper` re-exports `b::helper`, which re-exports `a::helper`, with no actual definition.
