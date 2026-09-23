module Compiler.TypeChecker.Data

import public Compiler.ScopeAndNameResolution.Data

import Frontend.ASTData
import Frontend.ASTPhases
import Frontend.Syntax.AST
import Frontend.Type

import Data.SortedMap

%default total

public export
TypedSymbolInfo : Type
TypedSymbolInfo = SymbolInfo (Maybe LeafType)

-- ReferenceRole describes syntax; AccessKind describes the typed operation on
-- a value. Neither records flow-dependent facts such as whether it was already
-- moved or whether this is its last use; those belong to ownership analysis.
public export
data AccessKind
  = ReadAccess          -- Observe/copy a value without transferring ownership.
  | WriteAccess         -- Initialize or replace a value.
  | MoveAccess          -- Transfer ownership elsewhere.
  | SharedBorrowAccess  -- Borrow without allowing mutation through this borrow.
  | MutableBorrowAccess -- Borrow with permission to mutate through this borrow.
  | ConsumeAccess       -- Consume a resource without transferring it onward.

public export
data PlaceRoot
  = LocalRoot SymbolId   -- A local variable or parameter, including a generated local.
  | TemporaryRoot NodeId -- Temporary storage produced by an expression, such as f() in f().x.

-- Projections are applied from left to right, starting at the place's root.
public export
data PlaceProjection
  = FieldProjection SymbolId -- The resolved field declaration, not its written spelling.
  | TupleProjection Nat      -- A zero-based tuple element index.
  | IndexProjection NodeId   -- The expression computing an array/slice index or range.
  | DereferenceProjection    -- The storage reached through a reference.

-- A field SymbolId alone identifies a declaration, not a particular value's
-- storage. For p.x, retain LocalRoot p and [FieldProjection x].
-- Different index-expression IDs or reference roots do not prove disjointness:
-- ownership analysis must account for dynamic indices and reference aliasing.
public export
record Place where
  constructor MkPlace
  root        : PlaceRoot
  projections : List PlaceProjection

public export
record AccessEvent where
  constructor MkAccessEvent
  kind       : AccessKind -- Classified using types and the operation's semantics.
  place      : Place      -- The affected local, temporary, field, element or referent.
  occurrence : AstInfo    -- The access's AST occurrence and diagnostic source span.

-- TypedModule repeats most of ResolvedModule's fields rather than embedding
-- a ResolvedModule value or sharing a parameterized base record. Idris
-- records have no subtyping/structural extension, so a literal field-list
-- repeat is the idiomatic way to add fields here -- but the repetition is
-- not purely mechanical, since not every field means the same thing on
-- both sides of typechecking:
--
--   * nodeScopes, scopes, memberScopes are expected to be threaded through
--     UNCHANGED from the ResolvedModule that fed this TypedModule: the
--     scope tree itself does not change during typechecking, only what
--     gets looked up in it (e.g. field/method resolution, which reuses the
--     same ScopeInfo/ScopeBinding machinery -- see MemberNameFor in
--     Syntax/Name.idr).
--   * references and localVariables are expected to grow: typechecking
--     adds the FieldReference/MethodReference entries pure lexical
--     resolution could not produce (it did not yet know the receiver's
--     type), and can fill in a qubit local's deferred linear/affine
--     default (see the comment on LocalVariableInfo.quantumStorage in
--     ScopeAndNameResolution/Data.idr).
--   * symbols and ast are genuinely different types at each phase
--     (TypedSymbolInfo vs ResolvedSymbolInfo, TypedSourceFile vs
--     ResolvedSourceFile) -- the one place the repetition is unavoidable
--     regardless of how the rest is factored.
--
-- None of this is enforced by the type system: nothing currently stops a
-- caller building a TypedModule whose nodeScopes/scopes/memberScopes
-- diverge from the ResolvedModule it was supposedly built from.
public export
record TypedModule where
  constructor MkTypedModule
  rootModule      : SymbolId
  rootScope       : ScopeId
  ast             : TypedSourceFile
  nodeScopes      : SortedMap NodeId ScopeId   -- lexical environment used to resolve a node
  symbols         : SortedMap SymbolId TypedSymbolInfo
  localVariables  : SortedMap SymbolId LocalVariableInfo
  references      : SortedMap SymbolId (SnocList SymbolReference)
  scopes          : SortedMap ScopeId ScopeInfo
  memberScopes    : SortedMap SymbolId ScopeId -- I have Point’s SymbolId. Which ScopeId contains its members?
  expressionTypes : SortedMap NodeId LeafType
  -- Events directly attributed to an expression/statement, in local evaluation
  -- order; accesses recorded on child nodes must not be duplicated here.
  -- Compile-time-only references (e.g. path qualifiers) produce no access event.
  -- SortedMap key order is NOT execution order: a later control-flow analysis
  -- determines legal moves/borrows, last uses and required cleanup.
  accesses        : SortedMap NodeId (List AccessEvent)
