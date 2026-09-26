module Compiler.ScopeAndNameResolution.Validate

import Compiler.ScopeAndNameResolution.Data
import Frontend.ASTData

import Data.List
import Data.Maybe
import Data.SnocList
import Data.SortedMap

%default total

-- These diagnostics describe inconsistent compiler output, not invalid Leaf
-- source. Keep them separate from ResolutionError and out of ASTData.
public export
data BindingNamespace = TypeNamespace | ValueNamespace | FieldNamespace

public export
Show BindingNamespace where
  show TypeNamespace = "type"
  show ValueNamespace = "value"
  show FieldNamespace = "field"

public export
data ResolutionInvariantError
  = MissingRootModule SymbolId
  | InvalidRootModule SymbolId
  | MissingRootScope ScopeId
  | InvalidRootScope ScopeId
  | SymbolKeyMismatch SymbolId SymbolId
  | MissingDeclaringScope SymbolId ScopeId
  | MissingDeclaringModule SymbolId SymbolId
  | InvalidDeclaringModule SymbolId SymbolId
  | ScopeKeyMismatch ScopeId ScopeId
  | MissingParentScope ScopeId ScopeId
  | ScopeParentCycle ScopeId
  | BindingNameMismatch ScopeId BindingNamespace String String
  | MissingBindingTarget ScopeId BindingNamespace String SymbolId
  | MissingDeclaredSymbol ScopeId SymbolId
  | DuplicateDeclaredSymbol ScopeId SymbolId
  | WrongDeclarationScope ScopeId SymbolId ScopeId
  | MissingDeclarationOrderEntry ScopeId SymbolId
  | MissingReferenceSymbol SymbolId
  | ReferenceKeyMismatch SymbolId SymbolId NodeId
  | MissingReferenceScope NodeId ScopeId
  | MissingNodeScope NodeId ScopeId
  | MissingLocalSymbol SymbolId
  | InvalidLocalSymbolKind SymbolId
  | MissingMemberOwner SymbolId
  | InvalidMemberOwner SymbolId
  | MissingMemberScope SymbolId ScopeId
  | InvalidMemberScopeKind SymbolId ScopeId

symbolLabel : SymbolId -> String
symbolLabel (MkSymbolId id) = "symbol #" ++ show id

scopeLabel : ScopeId -> String
scopeLabel (MkScopeId id) = "scope #" ++ show id

nodeLabel : NodeId -> String
nodeLabel (MkNodeId surfaceId desugarId) =
  "node #" ++ show surfaceId ++ "." ++ show desugarId

public export
Show ResolutionInvariantError where
  show (MissingRootModule id) = "Root module is missing: " ++ symbolLabel id
  show (InvalidRootModule id) = "Root module is not a module: " ++ symbolLabel id
  show (MissingRootScope id) = "Root scope is missing: " ++ scopeLabel id
  show (InvalidRootScope id) = "Root scope is not a ModuleScope: " ++ scopeLabel id
  show (SymbolKeyMismatch key stored) =
    "Symbol table key " ++ symbolLabel key ++ " contains " ++ symbolLabel stored
  show (MissingDeclaringScope symbol scope) =
    symbolLabel symbol ++ " has missing declaring " ++ scopeLabel scope
  show (MissingDeclaringModule symbol declaringModule) =
    symbolLabel symbol ++ " has missing declaring module " ++ symbolLabel declaringModule
  show (InvalidDeclaringModule symbol declaringModule) =
    symbolLabel symbol ++ " has non-module declaring symbol " ++ symbolLabel declaringModule
  show (ScopeKeyMismatch key stored) =
    "Scope table key " ++ scopeLabel key ++ " contains " ++ scopeLabel stored
  show (MissingParentScope scope parent) =
    scopeLabel scope ++ " has missing parent " ++ scopeLabel parent
  show (ScopeParentCycle scope) = "Scope parent cycle through " ++ scopeLabel scope
  show (BindingNameMismatch scope bindingNamespace key stored) =
    scopeLabel scope ++ " has " ++ show bindingNamespace ++ " binding key " ++ show key ++
      " with written name " ++ show stored
  show (MissingBindingTarget scope bindingNamespace name target) =
    scopeLabel scope ++ " has " ++ show bindingNamespace ++ " binding " ++ show name ++
      " targeting missing " ++ symbolLabel target
  show (MissingDeclaredSymbol scope symbol) =
    scopeLabel scope ++ " lists missing declaration " ++ symbolLabel symbol
  show (DuplicateDeclaredSymbol scope symbol) =
    scopeLabel scope ++ " lists declaration twice: " ++ symbolLabel symbol
  show (WrongDeclarationScope scope symbol declaringScope) =
    scopeLabel scope ++ " lists " ++ symbolLabel symbol ++ " declared in " ++ scopeLabel declaringScope
  show (MissingDeclarationOrderEntry scope symbol) =
    scopeLabel scope ++ " omits declared binding " ++ symbolLabel symbol ++ " from declaration order"
  show (MissingReferenceSymbol symbol) =
    "Reference table mentions missing " ++ symbolLabel symbol
  show (ReferenceKeyMismatch key target occurrence) =
    "Reference bucket " ++ symbolLabel key ++ " contains target " ++ symbolLabel target ++
      " at " ++ nodeLabel occurrence
  show (MissingReferenceScope occurrence scope) =
    "Reference at " ++ nodeLabel occurrence ++ " has missing enclosing " ++ scopeLabel scope
  show (MissingNodeScope node scope) =
    nodeLabel node ++ " is mapped to missing " ++ scopeLabel scope
  show (MissingLocalSymbol symbol) =
    "Local-variable metadata targets missing " ++ symbolLabel symbol
  show (InvalidLocalSymbolKind symbol) =
    "Local-variable metadata targets a non-local/non-parameter " ++ symbolLabel symbol
  show (MissingMemberOwner symbol) = "Member-scope owner is missing: " ++ symbolLabel symbol
  show (InvalidMemberOwner symbol) = "Symbol cannot own a member scope: " ++ symbolLabel symbol
  show (MissingMemberScope owner scope) =
    symbolLabel owner ++ " has missing member " ++ scopeLabel scope
  show (InvalidMemberScopeKind owner scope) =
    symbolLabel owner ++ " has incompatible member scope kind at " ++ scopeLabel scope

hasKey : Ord key => key -> SortedMap key value -> Bool
hasKey key entries = isJust (lookup key entries)

check : Bool -> ResolutionInvariantError -> List ResolutionInvariantError
check True _ = []
check False err = [err]

isModule : SymbolKind -> Bool
isModule SymbolModule = True
isModule _ = False

isLocal : SymbolKind -> Bool
isLocal SymbolLocalBinding = True
isLocal SymbolFunctionParameter = True
isLocal SymbolSelfReceiverParameter = True
isLocal _ = False

canOwnMembers : SymbolKind -> Bool
canOwnMembers SymbolModule = True
canOwnMembers SymbolStruct = True
canOwnMembers SymbolEnum = True
canOwnMembers SymbolQEnum = True
canOwnMembers SymbolEnumVariant = True
canOwnMembers _ = False

-- Modules may reuse ModuleScope instead of allocating a separate MemberScope.
-- SymbolKind alone cannot distinguish a struct-like variant from a tuple variant.
memberScopeMatches : SymbolKind -> ScopeKind -> Bool
memberScopeMatches SymbolModule ModuleScope = True
memberScopeMatches _ MemberScope = True
memberScopeMatches _ _ = False

validateRoots : ResolvedModule -> List ResolutionInvariantError
validateRoots resolved =
  let symbolErrors = case lookup resolved.rootModule resolved.tables.symbols of
        Nothing => [MissingRootModule resolved.rootModule]
        Just symbol => check (isModule symbol.symbolKind) (InvalidRootModule resolved.rootModule)
      scopeErrors = case lookup resolved.rootScope resolved.tables.scopes of
        Nothing => [MissingRootScope resolved.rootScope]
        Just scope => case scope.kind of
          ModuleScope => []
          _ => [InvalidRootScope resolved.rootScope]
  in symbolErrors ++ scopeErrors

validateSymbol : ResolvedModule -> (SymbolId, ResolvedSymbolInfo) -> List ResolutionInvariantError
validateSymbol resolved (key, symbol) =
  check (key == symbol.symbolId) (SymbolKeyMismatch key symbol.symbolId) ++
  check (hasKey symbol.declaringScope resolved.tables.scopes) (MissingDeclaringScope key symbol.declaringScope) ++
  case lookup symbol.declaringModule resolved.tables.symbols of
    Nothing => [MissingDeclaringModule key symbol.declaringModule]
    Just declaringModule =>
      check (isModule declaringModule.symbolKind) (InvalidDeclaringModule key symbol.declaringModule)

validateBinding : ResolvedModule -> ScopeId -> SortedMap SymbolId () -> BindingNamespace ->
                  (String, ScopeBinding) -> List ResolutionInvariantError
validateBinding resolved scope ordered bindingNamespace (name, binding) =
  check (name == binding.writtenName) (BindingNameMismatch scope bindingNamespace name binding.writtenName) ++
  case lookup binding.target resolved.tables.symbols of
    Nothing => [MissingBindingTarget scope bindingNamespace name binding.target]
    Just symbol => case binding.bindingKind of
      DeclaredBinding =>
        -- An impl member can be exposed in a member scope different from its
        -- lexical declaring scope; declaration order belongs to the latter.
        if symbol.declaringScope == scope
           then check (hasKey binding.target ordered) (MissingDeclarationOrderEntry scope binding.target)
           else []
      _ => []

validateDeclarationOrder : ResolvedModule -> ScopeId -> SortedMap SymbolId () ->
                           List SymbolId -> List ResolutionInvariantError
validateDeclarationOrder _ _ _ [] = []
validateDeclarationOrder resolved scope seen (symbolId :: rest) =
  let duplicateErrors = check (not (hasKey symbolId seen)) (DuplicateDeclaredSymbol scope symbolId)
      symbolErrors = case lookup symbolId resolved.tables.symbols of
        Nothing => [MissingDeclaredSymbol scope symbolId]
        Just symbol => check (symbol.declaringScope == scope)
                             (WrongDeclarationScope scope symbolId symbol.declaringScope)
  in duplicateErrors ++ symbolErrors ++
     validateDeclarationOrder resolved scope (insert symbolId () seen) rest

validateScope : ResolvedModule -> (ScopeId, ScopeInfo) -> List ResolutionInvariantError
validateScope resolved (key, scope) =
  let declarations = scope.declaredSymbolsInOrder <>> []
      ordered = fromList (map (\id => (id, ())) declarations)
      parentErrors = case scope.parent of
        Nothing => []
        Just parent => check (hasKey parent resolved.tables.scopes) (MissingParentScope key parent)
  in check (key == scope.id) (ScopeKeyMismatch key scope.id) ++ parentErrors ++
     concatMap (validateBinding resolved key ordered TypeNamespace) (SortedMap.toList scope.typeBindings) ++
     concatMap (validateBinding resolved key ordered ValueNamespace) (SortedMap.toList scope.valueBindings) ++
     concatMap (validateBinding resolved key ordered FieldNamespace) (SortedMap.toList scope.fieldBindings) ++
     validateDeclarationOrder resolved key empty declarations

-- A parent walk visits at most the number of scopes before stopping or repeating
-- a scope. Explicit fuel makes this total even on cyclic input. Completed walks
-- are memoized so shared parent chains are not retraversed for every scope.
walkParents : Nat -> SortedMap ScopeId ScopeInfo -> SortedMap ScopeId () ->
              SortedMap ScopeId () -> ScopeId -> (SortedMap ScopeId (), List ResolutionInvariantError)
walkParents Z _ done _ _ = (done, [])
walkParents (S fuel) scopes done active current =
  if hasKey current active then (done, [ScopeParentCycle current])
  else if hasKey current done then (done, [])
  else case lookup current scopes of
    Nothing => (done, []) -- Missing parents are reported by validateScope.
    Just scope =>
      let (completed, errors) = case scope.parent of
            Nothing => (done, [])
            Just parent => walkParents fuel scopes done (insert current () active) parent
      in (insert current () completed, errors)

validateParentCycles : SortedMap ScopeId ScopeInfo -> List ResolutionInvariantError
validateParentCycles scopes =
  let entries = SortedMap.toList scopes
      fuel = S (List.length entries)
      (_, errors) = foldl (visit fuel) (empty, []) entries
  in errors
  where
    visit : Nat -> (SortedMap ScopeId (), List ResolutionInvariantError) ->
            (ScopeId, ScopeInfo) -> (SortedMap ScopeId (), List ResolutionInvariantError)
    visit fuel (done, errors) (scope, _) =
      let (completed, newErrors) = walkParents fuel scopes done empty scope
      in (completed, newErrors ++ errors)

validateReference : ResolvedModule -> SymbolId -> SymbolReference -> List ResolutionInvariantError
validateReference resolved key reference =
  let targetErrors = if reference.target == key then [] else
        ReferenceKeyMismatch key reference.target reference.occurrence.nodeId ::
          check (hasKey reference.target resolved.tables.symbols) (MissingReferenceSymbol reference.target)
  in targetErrors ++ check (hasKey reference.enclosingScope resolved.tables.scopes)
                          (MissingReferenceScope reference.occurrence.nodeId reference.enclosingScope)

validateReferences : ResolvedModule -> (SymbolId, SnocList SymbolReference) -> List ResolutionInvariantError
validateReferences resolved (key, references) =
  check (hasKey key resolved.tables.symbols) (MissingReferenceSymbol key) ++
  concatMap (validateReference resolved key) (references <>> [])

validateLocal : ResolvedModule -> (SymbolId, LocalVariableInfo) -> List ResolutionInvariantError
validateLocal resolved (key, _) =
  case lookup key resolved.tables.symbols of
    Nothing => [MissingLocalSymbol key]
    Just symbol => check (isLocal symbol.symbolKind) (InvalidLocalSymbolKind key)

validateMembers : ResolvedModule -> (SymbolId, ScopeId) -> List ResolutionInvariantError
validateMembers resolved (owner, scopeId) =
  let ownerErrors = case lookup owner resolved.tables.symbols of
        Nothing => [MissingMemberOwner owner]
        Just symbol => check (canOwnMembers symbol.symbolKind) (InvalidMemberOwner owner)
      scopeErrors = case lookup scopeId resolved.tables.scopes of
        Nothing => [MissingMemberScope owner scopeId]
        Just scope => case lookup owner resolved.tables.symbols of
          Nothing => []
          Just symbol =>
            check (memberScopeMatches symbol.symbolKind scope.kind) (InvalidMemberScopeKind owner scopeId)
  in ownerErrors ++ scopeErrors

||| Check the stored resolution tables; an empty list means these invariants hold.
||| This does not re-resolve names, check visibility, require a dense nodeScopes
||| table, or prove that every AST occurrence was recorded. Member references may
||| still be deferred to typechecking. Qualifier occurrences may retain NodeIds
||| absent from the collapsed resolved AST. Historical shadowed declarations are
||| valid even when no current name-map entry points to them. Declaration order
||| is checked for membership/duplicates, not reconstructed from source spans.
public export
validateResolvedModule : ResolvedModule -> List ResolutionInvariantError
validateResolvedModule resolved =
  validateRoots resolved ++
  concatMap (validateSymbol resolved) (SortedMap.toList resolved.tables.symbols) ++
  concatMap (validateScope resolved) (SortedMap.toList resolved.tables.scopes) ++
  validateParentCycles resolved.tables.scopes ++
  concatMap (validateReferences resolved) (SortedMap.toList resolved.tables.references) ++
  concatMap (\(node, scope) => check (hasKey scope resolved.tables.scopes) (MissingNodeScope node scope))
            (SortedMap.toList resolved.tables.nodeScopes) ++
  concatMap (validateLocal resolved) (SortedMap.toList resolved.tables.localVariables) ++
  concatMap (validateMembers resolved) (SortedMap.toList resolved.tables.memberScopes)
