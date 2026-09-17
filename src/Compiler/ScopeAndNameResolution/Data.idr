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

public export
data ResolutionError
  = UnresolvedName String SourceSpan                         -- No matching declaration is visible at the name occurrence.
  | IllegalShadowing String SymbolId SourceSpan SourceSpan   -- Name illegally shadows the identified symbol; original
                                                             --  and shadowing spans.
  | AmbiguousName String (List SymbolId) SourceSpan          -- Multiple accessible symbols match the name occurrence.
  | UnresolvedPathSegment String String SourceSpan           -- A component of the qualified path cannot be resolved.
  | InaccessibleSymbol String SymbolId SourceSpan            -- The symbol exists but is not visible from the current context.
  | DuplicatePatternBinding String SourceSpan SourceSpan     -- Name is bound twice in one pattern; first and duplicate spans.
