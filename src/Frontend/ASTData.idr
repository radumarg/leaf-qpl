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

-- Unique id for a declared entity, not for each name bound to it in a scope.
-- Imports and aliases introduce ScopeBindings that retain the target entity's
-- SymbolId; they do not create a new identity for that entity.
-- Not every AST node declares an entity, and some entities (such as prelude
-- symbols) have no source declaration node.
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

-- Unique id for a lexical scope or a member namespace.
-- Blocks, functions, and modules can introduce lexical scopes; types and
-- struct-like enum variants can also own scopes containing their members.
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
  | DesugaredFieldShorthand
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
  show DesugaredFieldShorthand = "expanded shorthand field"
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
-- A function TYPE's own parameter names (`fn(qs: [qubit; 4]) -> ...` used as
-- a type, not a declaration) also have no SymbolKind here: they never
-- resolve to a symbol at all and stay plain written text at every AST phase
-- -- see the comment on FunctionTypeParameterNode in Syntax/Type.idr.
public export
data SymbolKind
  = SymbolLocalBinding          -- A binder introduced by let, for, match, or qmatch.
  | SymbolFunctionParameter     -- An ordinary named parameter of a function declaration.
  | SymbolSelfReceiverParameter -- The self, &self, or &mut self parameter of a method.
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
  | PreludeSymbol                -- A symbol with no ExprBuiltin declaration in the source AST, like a prelude functions.
  | GeneratedSymbol AstInfo      -- A compiler-generated symbol; AstInfo identifies the generated declaration node and its span.

public export
data SymbolVisibility
  = PublicVisibility            -- May be accessed from another module, subject to path accessibility.
  | ModuleVisibility            -- Access is restricted by the declaring module's privacy rules.
  | LexicalVisibility           -- Local binding: accessible only through lexical scope.

public export
record SymbolInfo (typeInfo : Type) where
  constructor MkSymbolInfo
  symbolId         : SymbolId
  symbolKind       : SymbolKind
  symbolType       : typeInfo
  declaredName     : String
  declaringScope   : ScopeId
  declaringModule  : SymbolId
  visibility       : SymbolVisibility
  origin           : SymbolOrigin

public export
data ReferenceRole
  = ValueReference       -- x in x + 1, f(x), or return x.
  | AssignmentTarget     -- x in x = value.
  | TypeReference        -- Point in a type annotation, cast, or impl target.
  | QualifierReference   -- math in math::calculate.
  | ImportReference      -- calculate in use math::calculate.
  | ConstructorReference -- Point in Point { x: 1 }.
  | PatternReference     -- Left in a matching pattern such as Result::Left(x).
  | FieldReference       -- x in point.x, Point { x: value }, or a struct pattern.
  | MethodReference      -- update in object.update().

public export
record SymbolReference where
  constructor MkSymbolReference
  target         : SymbolId
  occurrence     : AstInfo
  enclosingScope : ScopeId           -- block, function, or descendant scopes.
  contextNode    : NodeId            -- expression node, value assignment node, type annotation node, import path node
  role           : ReferenceRole     -- expression vs value assignment vs type annotation vs import path
  writtenName    : String            -- spelling used here, for import aliases this may be different from symbol declaredName

--------------------------------------------------------------------------------
-- Scope information
--
-- Scope data live in a scope tree / resolver output, not directly on every AST node.
--------------------------------------------------------------------------------

public export
data ScopeOrigin          -- Describes how the entire scope was created.
  = SourceScope AstInfo   -- A source-backed lexical scope or member namespace.
  | PreludeScope          -- The compiler-created scope containing names made available by the language prelude.
  | ExternalModuleScope   -- The scope representing the exported namespace of a module defined outside this source file.

public export
data BindingKind     -- Describes how one particular name entered that scope.
  = DeclaredBinding  -- Declaration-provided, not imported; may expose an impl member outside its declaring scope.
  | ImportedBinding  -- Introduced by an explicit import into this scope.
  | PreludeBinding   -- Made available implicitly through the language prelude.

public export
record ScopeBinding where
  constructor MkScopeBinding
  writtenName       : String
  introducedAt      : Maybe SourceSpan
  target            : SymbolId
  bindingKind       : BindingKind
  -- Controls access through this binding, independently of the target's declaration visibility.
  -- ModuleVisibility is relative to the module containing this binding's scope,
  -- not the target symbol's declaringModule. A private use of a public symbol
  -- therefore keeps the imported name private without changing the target symbol.
  bindingVisibility : SymbolVisibility

public export
data ScopeKind
  = ModuleScope       -- Top-level source module or a nested module namespace.
  | FunctionScope     -- Function or method parameters, including a self receiver; holds parameters, its body can have a child BlockScope.
  | BlockScope        -- Local declarations inside a block. Includes if, while, loop, and quantum-control block.
  | ForScope          -- Pattern bindings introduced by a for loop, visible in its body.
  | MatchArmScope     -- Pattern bindings belonging to one classical or quantum match arm.
  | ImplScope         -- Lexical context for resolving declarations inside an impl block.
  | MemberScope       -- Members owned by a module, type, or struct-like enum variant.

public export
record ScopeInfo where
  constructor MkScopeInfo
  id                      : ScopeId
  kind                    : ScopeKind
  -- Lexical/enclosing context, not member ownership. Unqualified lookup may
  -- follow this link subject to the language's scope and capture rules.
  -- Member ownership is recorded separately by the module's memberScopes table,
  -- which maps an owning SymbolId to the ScopeId used for qualified lookup.
  -- Qualified lookup searches the appropriate name map in that scope without
  -- falling back through parent: Point::missing must not find an enclosing
  -- module's unrelated missing declaration.
  -- A module's memberScopes entry can reuse its existing ModuleScope; it does
  -- not need a second scope solely for qualified lookup.
  parent                  : Maybe ScopeId
  origin                  : ScopeOrigin
  -- Each name map holds one binding per spelling. When same-scope shadowing is
  -- allowed, the newer binding replaces the entry; the completed map is not a
  -- history of which binding was visible at each source position. Resolve each
  -- occurrence in the environment valid there and preserve its target SymbolId
  -- in the AST/reference table, so later shadowing cannot change earlier uses.
  -- declaredSymbolsInOrder alone does not reconstruct those earlier environments;
  -- source-position lookup would need binding history or scope snapshots.
  typeBindings           : SortedMap String ScopeBinding
  valueBindings          : SortedMap String ScopeBinding
  fieldBindings          : SortedMap String ScopeBinding   -- Rust treats fields separately from ordinary namespaces
  declaredSymbolsInOrder : SnocList SymbolId
