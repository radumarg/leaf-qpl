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
record LocalVariableInfo where
  constructor MkLocalVariableInfo
  mutability : LocalMutability
  -- Nothing is overloaded with two different meanings that this type alone
  -- cannot distinguish:
  --   (a) this local's type isn't qubit-shaped at all, so no qubit storage
  --       qualifier applies -- e.g. an i32 or a struct;
  --   (b) this local IS qubit-shaped (directly, or as an array/tuple
  --       element) but its linear/affine/scratch default has not been
  --       assigned yet -- that only happens once typechecking knows the
  --       type and fills in the default (linear, non-scratch) for an
  --       unqualified `let`.
  -- Telling (a) from (b) apart means cross-referencing this same SymbolId's
  -- SymbolInfo.symbolType in the enclosing ResolvedModule/TypedModule's
  -- `symbols` table. Nothing here or in Validate.idr checks that the two
  -- tables actually agree, so a bug that leaves a qubit local's (b) as
  -- Nothing past typechecking would look identical to a legitimately
  -- non-qubit local.
  quantumStorage : Maybe QuantumStorageProperties

-- See the comment on TypedModule in Compiler/TypeChecker/Data.idr for how
-- these tables are expected to evolve once typechecking builds a
-- TypedModule from this one.
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
