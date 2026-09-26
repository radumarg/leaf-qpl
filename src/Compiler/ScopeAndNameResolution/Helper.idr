module Compiler.ScopeAndNameResolution.Helper

import Compiler.ScopeAndNameResolution.Data
import Frontend.ASTData

import Control.Monad.State
import Data.SortedMap

export
registerNewScope : (NodeId, SymbolId, ScopeId ) -> State ScopeTables ()
registerNewScope (nodeId, symbolId, scopeId) = do
  scopeTables <- get
  put $ MkScopeTables
    (insert nodeId scopeId scopeTables.nodeScopes)
    empty
    empty
    empty
    empty
    empty
  pure ()
