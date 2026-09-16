module Frontend.ASTData

import Frontend.Source

%default total

--------------------------------------------------------------------------------
-- Compiler ids
--------------------------------------------------------------------------------

-- Unique node id for each AST node.
-- surfaceId is incremented by the Parser.
-- Parser also assigns desugarId 0 as default
-- for each node, which will be incremented on 
-- nodes generated during desugaring
public export
record NodeId where
  constructor MkNodeId
  surfaceId : Nat
  desugarId: Nat

-- Unique id for a name/binding, 
-- introduced by the program.
-- Not all nodes introduce a name.
-- Not all names are introduced by a node, 
-- e.g. builtins, imports are not.
public export
record SymbolId where
  constructor MkSymbolId
  id : Nat

-- Blocks, functions, modules, 
-- can introduce scopes.
-- Unique id for a lexical scope.
public export
record ScopeId where
  constructor MkScopeId
  id : Nat

--------------------------------------------------------------------------------
-- Node provenance: written/desugaring/type-checker
--------------------------------------------------------------------------------

public export
data NodeProvenance
  = Written
  | DefaultElseBlock
  | DefaultUnitValue
  | DesugaredExpression
  | DesugaredAssignment
  | DesugaredReturnStatement
  | InferredAttributeArgument
  | InferredDefaultFunctionEffect
  | InferredDefaultFunctionReturnType
  | InferredDefaultQubitQualifier


public export
Show NodeProvenance where
  show DefaultElseBlock = "default empty else block"
  show DefaultUnitValue = "default unit value"
  show DesugaredExpression = "desugared expression"
  show DesugaredAssignment = "desugared assignment from compound assignment"
  show DesugaredReturnStatement = "desugared return statement"
  show InferredAttributeArgument = "inferred attribute argument"
  show InferredDefaultFunctionEffect = "inferred default function effect"
  show InferredDefaultFunctionReturnType = "inferred default function return type"
  show InferredDefaultQubitQualifier = "inferred default qubit qualifier"
  show Written = "user written code"

--------------------------------------------------------------------------------
-- Common AST information
--------------------------------------------------------------------------------

public export
record AstInfo where
  constructor MkAstInfo
  nodeId : NodeId
  span   : SourceSpan

--------------------------------------------------------------------------------
-- Scope information
--
-- Scope data live in a scope tree / resolver output, not directly on every AST node.
-- Nodes that introduce scopes carry their own ScopeId in their payload.
--------------------------------------------------------------------------------

public export
record Scope where
  constructor MkScope
  id      : ScopeId
  parent  : Maybe ScopeId
  symbols : List SymbolId

--------------------------------------------------------------------------------
-- Helpers for AST nodes
--
-- Creates a NodeId with current Id, and returns the next Id for the next node.
--------------------------------------------------------------------------------

public export
reserveNodeId : Nat -> (NodeId, Nat)
reserveNodeId current = (MkNodeId current 0, S current)
