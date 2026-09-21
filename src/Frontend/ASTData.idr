module Frontend.ASTData

import Frontend.Source

import Data.SortedMap
import Data.SnocList

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

public export
Eq NodeId where
  MkNodeId leftSurface leftDesugar == MkNodeId rightSurface rightDesugar =
    leftSurface == rightSurface && leftDesugar == rightDesugar

public export
Ord NodeId where
  compare (MkNodeId leftSurface leftDesugar) (MkNodeId rightSurface rightDesugar) =
    case compare leftSurface rightSurface of
      EQ => compare leftDesugar rightDesugar
      ordering => ordering

-- Unique id for a name/binding, 
-- introduced by the program.
-- Not all nodes introduce a name.
-- Not all names are introduced by a node, 
-- e.g. builtins, imports are not.
public export
record SymbolId where
  constructor MkSymbolId
  id : Nat

public export
Eq SymbolId where
  MkSymbolId left == MkSymbolId right = left == right

public export
Ord SymbolId where
  compare (MkSymbolId left) (MkSymbolId right) = compare left right

-- Blocks, functions, modules, 
-- can introduce scopes.
-- Unique id for a lexical scope.
public export
record ScopeId where
  constructor MkScopeId
  id : Nat

public export
Eq ScopeId where
  MkScopeId left == MkScopeId right = left == right

public export
Ord ScopeId where
  compare (MkScopeId left) (MkScopeId right) = compare left right

--------------------------------------------------------------------------------
-- Node provenance: written/desugaring/type-checker
--------------------------------------------------------------------------------

public export
data NodeProvenance
  = WrittenCode
  | DesugaredElseBlock
  | DesugaredUnitValue
  | DesugaredExpression
  | DesugaredAssignment
  | DesugaredReturnStatement
  | DesugaredCtrlDefaultOnInvocation
  | DesugaredDefaultAttributeArgument
  | DesugaredDefaultFunctionEffect
  | DesugaredDefaultFunctionReturnType
  | DesugaredDefaultQubitQualifier


public export
Show NodeProvenance where
  show DesugaredElseBlock = "default empty else block"
  show DesugaredUnitValue = "default unit value"
  show DesugaredExpression = "desugared expression"
  show DesugaredAssignment = "desugared assignment from compound assignment"
  show DesugaredReturnStatement = "desugared return statement"
  show DesugaredCtrlDefaultOnInvocation = "desugared control syntax default on(bs\"1..\") invocation"
  show DesugaredDefaultAttributeArgument = "inferred attribute argument"
  show DesugaredDefaultFunctionEffect = "inferred default function effect"
  show DesugaredDefaultFunctionReturnType = "inferred default function return type"
  show DesugaredDefaultQubitQualifier = "inferred default qubit qualifier"
  show WrittenCode = "user written code"

--------------------------------------------------------------------------------
-- Common AST information
--------------------------------------------------------------------------------

public export
record AstInfo where
  constructor MkAstInfo
  nodeId : NodeId
  span   : SourceSpan

--------------------------------------------------------------------------------
-- Symbol information
--------------------------------------------------------------------------------

-- The declaration or binding category denoted by a SymbolId.
-- Reserved builtins are represented directly by ExprBuiltin and therefore do
-- not need a SymbolKind. Shadowable prelude functions are ordinary functions.
public export
data SymbolKind
  = SymbolLocalBinding          -- A binder introduced by let, for, match, or qmatch.
  | SymbolFunctionParameter     -- An ordinary named parameter of a function declaration.
  | SymbolSelfReceiverParameter -- The self, &self, or &mut self parameter of a method.
  | SymbolFunctionTypeParameter -- A named parameter appearing inside a function type.
  | SymbolConstant              -- A named const item.
  | SymbolFunction              -- A free function declared at module level.
  | SymbolAssociatedFunction    -- A function declared in an impl without a self receiver.
  | SymbolMethod                -- A function declared in an impl with a self receiver.
  | SymbolModule                -- An inline or external module declaration.
  | SymbolStruct                -- A named struct type and its value constructor.
  | SymbolField                 -- A named field of a struct or struct-like enum variant.
  | SymbolEnum                  -- A named classical enum type.
  | SymbolEnumVariant           -- A unit, tuple-like, or struct-like classical enum variant.
  | SymbolQEnum                 -- A named quantum enum type.
  | SymbolQEnumVariant          -- A tuple-like quantum enum variant.

public export
data SymbolOrigin
  = SourceSymbol AstInfo         -- A user-written declaration; AstInfo identifies its declaration node and source span.
  | ImportedSymbol SourceSpan    -- A symbol introduced by an import; SourceSpan identifies the import site.
  | PreludeSymbol                -- A symbol with no ExprBuiltin declaration in the source AST, like a prelude functions.
  | GeneratedSymbol AstInfo      -- A compiler-generated symbol; AstInfo identifies the generated declaration node and its span.

public export
record SymbolInfo (typeInfo : Type) where
  constructor MkSymbolInfo
  symbolId         : SymbolId
  symbolKind       : SymbolKind
  symbolType       : typeInfo
  declaredName     : String
  declarationInfo  : AstInfo
  declaringScope   : ScopeId
  origin           : SymbolOrigin

--------------------------------------------------------------------------------
-- Scope information
--
-- Scope data live in a scope tree / resolver output, not directly on every AST node.
-- Nodes that introduce scopes carry their own ScopeId in their payload.
--------------------------------------------------------------------------------

data ScopeOrigin          -- Describes how the entire scope was created.
  = SourceScope AstInfo   -- A lexical scope introduced by a source AST node, such as a module, function, or block.
  | PreludeScope          -- The compiler-created scope containing names made available by the language prelude.
  | ExternalModuleScope   -- The scope representing the exported namespace of a module defined outside this source file.

public export
data BindingKind     -- Describes how one particular name entered that scope.
  = DeclaredBinding  -- Introduced by a declaration directly within this scope.
  | ImportedBinding  -- Introduced by an explicit import into this scope.
  | PreludeBinding   -- Made available implicitly through the language prelude.

record ScopeBinding where
  constructor MkScopeBinding
  writtenName  : String
  introducedAt : SourceSpan
  target       : SymbolId
  bindingKind  : BindingKind

public export
record ScopeInfo where
  constructor MkScopeInfo
  id              : ScopeId
  parent          : Maybe ScopeId
  origin          : ScopeOrigin
  bindings        : SortedMap String ScopeBinding
  symbolsInOrder  : SnocList SymbolId

