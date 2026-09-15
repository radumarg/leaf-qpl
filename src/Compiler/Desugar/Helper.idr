module Compiler.Desugar.Helper

import Data.List1
import Frontend.ASTData
import Frontend.ASTPhases
import Frontend.Syntax.AST
import Frontend.Syntax.Name
import Frontend.Syntax.Type
import Frontend.Token

%default total

export
mapWithId :
  {a : Type} ->
  {b : Type} ->
  (fun : Nat -> a -> (b, Nat)) ->
  (startingId : Nat) ->
  (values : List a) ->
  (List b, Nat)
mapWithId fun fstId [] = ([], fstId)
mapWithId fun fstId (x :: xs) =
  let (result, sndId) = fun fstId x
      (remaining, finalId) = mapWithId fun sndId xs
  in (result :: remaining, finalId)

export
incrementedAstInfo : AstInfo -> Nat -> AstInfo
incrementedAstInfo astInfo inc =
  MkAstInfo (MkNodeId astInfo.nodeId.surfaceId (astInfo.nodeId.desugarId + inc)) astInfo.span

export
isQubitLikeType : Maybe SurfaceTy -> Bool
isQubitLikeType Nothing = False
isQubitLikeType (Just ty) = isQubitLike ty
  where
    isQubitLike : SurfaceTy -> Bool
    isQubitLike (MkAstNode _ _ typeNode) =
      case typeNode of
        TyPrimitive primitiveName => primitiveName == TypPrimQubit
        TyPath _ => False
        TyUnit => False
        TyParenthesized innerType => recur innerType
        TyTuple elementTypes => any recur elementTypes
        TyArray elementType _ => recur elementType
        TySlice elementType => recur elementType
        TyReference _ _ => False
        TyQualified _ qualifiedType => recur qualifiedType
        TyFunction _ _ _ => False
      where
        recur : SurfaceTy -> Bool
        recur nestedType =
          isQubitLike (assert_smaller typeNode nestedType)
