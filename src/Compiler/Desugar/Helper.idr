module Compiler.Desugar.Helper

import Frontend.ASTData
import Frontend.ASTPhases
import Frontend.Syntax.Name

%default total

export
mapWithId :
  {a : Type} ->
  {b : Type} ->
  (fun : Nat -> a -> (b, Nat)) ->
  (startingId : Nat) ->
  (values : List a) ->
  (List b, Nat)
mapWithId fun nextId [] = ([], nextId)
mapWithId fun nextId (x :: xs) =
  let (result, afterResultId) = fun nextId x
      (remaining, finalId) = mapWithId fun afterResultId xs
  in (result :: remaining, finalId)

export
incrementedAstInfo : {0 a : Type} -> AstNode SurfaceAstPhase a -> Nat -> AstInfo
incrementedAstInfo (MkAstNode astInfo x value) inc =
  MkAstInfo (MkNodeId astInfo.nodeId.surfaceId (astInfo.nodeId.desugarId + inc)) astInfo.span
  
