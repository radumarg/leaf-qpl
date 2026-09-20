module Compiler.ScopeAndNameResolution.Data

import Frontend.ASTData
import Frontend.ASTPhases
import Frontend.Source
import Frontend.Syntax.AST


import Data.SortedMap

%default total

public export
ResolvedSymbolInfo : Type
ResolvedSymbolInfo = SymbolInfo ()

public export
record ResolvedProgram where
  constructor MkResolvedProgram
  ast         : ResolvedSourceFile
  rootScope   : ScopeId
  nodeScopes  : SortedMap NodeId ScopeId
  symbols     : SortedMap SymbolId ResolvedSymbolInfo
  scopes      : SortedMap ScopeId ScopeInfo

-- Covered by PostParseValidation:
--  DuplicatePatternBinding: one pattern introduces the same variable twice, example: `let (x, x) = pair;`.

public export
data ResolutionError
  = UnresolvedName String SourceSpan                         -- No matching declaration is visible at the use site
  | IllegalShadowing String SymbolId SourceSpan SourceSpan   -- A declaration shadows a protected name, example: `let measr = 1;`
                                                             -- error keeps contains original and shadowing spans.
  | DuplicateDeclaration String (List SymbolId) SourceSpan   -- Declarations conflict in the same scope and namespace,
                                                             -- example: two functions named `helper` in one module.
  | InaccessibleSymbol                                       -- Declaration exists but visibility rules prohibit access,
                                                             -- example: accessing a private function from a sibling module.
  | AmbiguousName String (List SymbolId) SourceSpan          -- Lookup finds multiple candidates without a rule selecting one,
                                                             -- example: calling `helper()` after wildcard imports expose two different `helper` functions.
  | DuplicatePatternBinding String SourceSpan SourceSpan     -- Name is bound twice in one pattern; first and duplicate spans.
  | IllegalCapture String SourceSpan                         -- Nested functions are forbidden from capturing enclosing locals,
                                                             -- example: `fn outer() { let x = 1; fn inner() -> i32 { x } }`
-- UnresolvedPathSegment, when a component of a qualified path cannot be found, example: `my_library::missing::helper`.
-- InvalidPathQualifier, when a resolved symbol cannot qualify another path component, example: `count::helper` where `count` is an integer variable.
-- ConflictingImports, when imports introduce different symbols under the same name where forbidden, example: `use a::helper; use b::helper;`.
-- WrongNamespace, when a name exists only in a namespace incompatible with its use, example: `let y: x = 0;` where `x` names only a variable.
-- UseBeforeDeclaration, optionally when a local variable is referenced before its declaration, example: `let y = x; let x = 1;` with no earlier `x` in scope.
-- ModuleNotFound, during module loading when an external module cannot be located, example: `mod my_library;` with no corresponding module source.
-- CyclicImport, when an import cycle prevents resolution or violates Leaf’s rules, example: `a::helper` re-exports `b::helper`, which re-exports `a::helper`, with no actual definition.
