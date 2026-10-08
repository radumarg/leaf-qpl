module Compiler.ScopeAndNameResolution.Helper

import Frontend.ASTData
import Frontend.ASTPhases
import Frontend.Syntax.Common

%default total

export
symbolVisibility : Maybe (AstNode CanonicalAstPhase VisibilityQualifier) -> SymbolVisibility
symbolVisibility (Just (MkAstNode _ _ VisibilityPublic)) = PublicVisibility
symbolVisibility Nothing = ModuleVisibility
