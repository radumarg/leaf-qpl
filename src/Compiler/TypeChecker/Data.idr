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

record TypedProgram where
  constructor MkTypedProgram
  ast         : TypedSourceFile
  symbols     : SortedMap SymbolId TypedSymbolInfo
  scopes      : SortedMap ScopeId ScopeInfo
