module Compiler.TypeChecker.Data

import Frontend.ASTData
import Frontend.ASTPhases
import Frontend.Syntax.AST
import Frontend.Type

import Data.SortedMap

%default total

public export
TypedSymbolInfo : Type
TypedSymbolInfo = SymbolInfo LeafType

public export
record TypedModule where
  constructor MkTypedModule
  ast             : TypedSourceFile
  rootScope       : ScopeId
  nodeScopes      : SortedMap NodeId ScopeId
  symbols         : SortedMap SymbolId TypedSymbolInfo
  scopes          : SortedMap ScopeId ScopeInfo
  expressionTypes : SortedMap NodeId LeafType
