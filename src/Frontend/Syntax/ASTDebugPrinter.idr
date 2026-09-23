module Frontend.Syntax.ASTDebugPrinter

import Data.List1

import Frontend.ASTData
import Frontend.ASTPhases
import Frontend.Token
import Frontend.Syntax.AST
import Frontend.Syntax.ASTPrettyPrinter
import Frontend.Syntax.Common
import Frontend.Syntax.Name
import Frontend.Syntax.Operator
import Frontend.Syntax.Pattern
import Frontend.Syntax.Type

-- This diagnostic traversal follows the same mutually-recursive graph as the
-- AST.  Its source fragments are still produced by the total strict printer.
%default covering

-- Structural output for inspecting the AST at ANY phase (surface, canonical,
-- resolved, typed): generic over `phase : AstPhase`, exactly like
-- ASTPrettyPrinter, and delegating every `strict = ...` fragment to that
-- module's phase-polymorphic PrettyStrict renderers (which add all
-- syntactically optional parentheses).  This module does not change the
-- source-oriented Show instances.

indent : Nat -> String
indent Z = ""
indent (S n) = "  " ++ indent n

astNodeNumber : AstInfo -> Nat
astNodeNumber (MkAstInfo (MkNodeId surfaceId desugarId) _) = surfaceId

node : Nat -> String -> String -> AstInfo -> String -> String
node depth ty ctor info strict =
  indent depth ++ "AstNode<" ++ ty ++ "> #" ++ show (astNodeNumber info) ++
  " " ++ ctor ++ "\n" ++ indent (S depth) ++ "strict = " ++ show strict ++ "\n"

field : Nat -> String -> String -> String
field depth name value = indent depth ++ name ++ " = " ++ show value ++ "\n"

nameText : PhasePretty phase => Name phase -> String
nameText (MkAstNode _ _ n) = prettyName n

memberNameText : PhasePretty phase => MemberName phase -> String
memberNameText (MkAstNode _ _ n) = prettyMemberName n

leafText : Show a => AstNode phase a -> String
leafText (MkAstNode _ _ value) = show value

exprCtor : ExpressionNode phase -> String
exprCtor e = case e of
  ExprLiteral _ => "ExprLiteral"
  ExprName _ => "ExprName"
  ExprPath _ => "ExprPath"
  ExprBuiltin _ => "ExprBuiltin"
  ExprSelf _ => "ExprSelf"
  ExprParenthesized _ => "ExprParenthesized"
  ExprTuple _ => "ExprTuple"
  ExprArray _ => "ExprArray"
  ExprRepeatedArray _ _ => "ExprRepeatedArray"
  ExprStructLiteral _ _ => "ExprStructLiteral"
  ExprCall _ _ => "ExprCall"
  ExprMethodCall _ _ _ => "ExprMethodCall"
  ExprField _ _ => "ExprField"
  ExprTupleIndex _ _ => "ExprTupleIndex"
  ExprIndex _ _ => "ExprIndex"
  ExprUnary _ _ => "ExprUnary"
  ExprBinary _ _ _ => "ExprBinary"
  ExprRange _ _ _ => "ExprRange"
  ExprCast _ _ => "ExprCast"
  ExprBlock _ => "ExprBlock"
  ExprIf _ => "ExprIf"
  ExprQIf _ => "ExprQIf"
  ExprSIf _ => "ExprSIf"
  ExprMatch _ => "ExprMatch"
  ExprQMatch _ => "ExprQMatch"
  ExprSMatch _ => "ExprSMatch"
  ExprLoop _ => "ExprLoop"
  ExprWhile _ _ => "ExprWhile"
  ExprFor _ _ _ => "ExprFor"
  ExprBreak _ => "ExprBreak"
  ExprContinue => "ExprContinue"
  ExprReturn _ => "ExprReturn"
  ExprCtrl _ => "ExprCtrl"
  ExprAdjoint _ => "ExprAdjoint"

mutual
  debugExpr : PhasePretty phase => Nat -> Expr phase -> String
  debugExpr depth expression@(MkAstNode info _ value) =
    node depth "ExpressionNode" (exprCtor value) info (showExprStrict expression) ++
    debugExprChildren (S depth) value

  debugExprChildren : PhasePretty phase => Nat -> ExpressionNode phase -> String
  debugExprChildren depth e = case e of
    ExprParenthesized x => debugExpr depth x
    ExprTuple xs => debugExprs depth (forget xs)
    ExprArray xs => debugExprs depth xs
    ExprRepeatedArray x n => debugExpr depth x ++ debugExpr depth n
    ExprCall f xs => debugExpr depth f ++ debugExprs depth xs
    ExprMethodCall x method xs =>
      field depth "method" (memberNameText method) ++ debugExpr depth x ++ debugExprs depth xs
    ExprField x name => field depth "field" (memberNameText name) ++ debugExpr depth x
    ExprTupleIndex x index => field depth "index" index ++ debugExpr depth x
    ExprIndex x index => debugExpr depth x ++ debugExpr depth index
    ExprUnary _ x => debugExpr depth x
    ExprBinary _ x y => debugExpr depth x ++ debugExpr depth y
    ExprRange x _ y => debugMaybeExpr depth x ++ debugMaybeExpr depth y
    ExprCast x ty => debugExpr depth x ++ debugTy depth ty
    ExprBlock block => debugBlock depth block
    ExprLoop block => debugBlock depth block
    ExprWhile condition block => debugExpr depth condition ++ debugBlock depth block
    ExprFor pattern iterable block =>
      field depth "pattern" (showPattern pattern) ++ debugExpr depth iterable ++ debugBlock depth block
    ExprBreak x => debugMaybeExpr depth x
    ExprReturn x => debugMaybeExpr depth x
    _ => ""

  debugExprs : PhasePretty phase => Nat -> List (Expr phase) -> String
  debugExprs _ [] = ""
  debugExprs depth (x :: xs) = debugExpr depth x ++ debugExprs depth xs

  debugMaybeExpr : PhasePretty phase => Nat -> Maybe (Expr phase) -> String
  debugMaybeExpr _ Nothing = ""
  debugMaybeExpr depth (Just x) = debugExpr depth x

  debugTy : PhasePretty phase => Nat -> Ty phase (Expr phase) -> String
  debugTy depth ty@(MkAstNode info _ value) = case value of
    TyPrimitive primitive =>
      node depth "TyNode Expr" "TyPrimitive" info (showTyStrict ty) ++
      field (S depth) "primitive" (showTypPrimLeaf primitive)
    TyPath _ => node depth "TyNode Expr" "TyPath" info (showTyStrict ty)
    TyUnit => node depth "TyNode Expr" "TyUnit" info (showTyStrict ty)
    TyParenthesized x =>
      node depth "TyNode Expr" "TyParenthesized" info (showTyStrict ty) ++ debugTy (S depth) x
    TyTuple xs =>
      node depth "TyNode Expr" "TyTuple" info (showTyStrict ty) ++ debugTys (S depth) (forget xs)
    TyArray element size =>
      node depth "TyNode Expr" "TyArray" info (showTyStrict ty) ++
      debugTy (S depth) element ++ debugExpr (S depth) size
    TySlice x => node depth "TyNode Expr" "TySlice" info (showTyStrict ty) ++ debugTy (S depth) x
    TyReference _ x => node depth "TyNode Expr" "TyReference" info (showTyStrict ty) ++ debugTy (S depth) x
    TyQualified _ x => node depth "TyNode Expr" "TyQualified" info (showTyStrict ty) ++ debugTy (S depth) x
    TyFunction _ _ result =>
      node depth "TyNode Expr" "TyFunction" info (showTyStrict ty) ++ debugMaybeTy (S depth) result

  debugTys : PhasePretty phase => Nat -> List (Ty phase (Expr phase)) -> String
  debugTys _ [] = ""
  debugTys depth (x :: xs) = debugTy depth x ++ debugTys depth xs

  debugMaybeTy : PhasePretty phase => Nat -> Maybe (Ty phase (Expr phase)) -> String
  debugMaybeTy _ Nothing = ""
  debugMaybeTy depth (Just x) = debugTy depth x

  debugBlock : PhasePretty phase => Nat -> Block phase -> String
  debugBlock depth block@(MkAstNode info _ (MkBlockNode _ statements final)) =
    node depth "BlockNode" "MkBlockNode" info (showBlockStrict block) ++
    debugStatements (S depth) statements ++ debugMaybeExpr (S depth) final

  debugStatements : PhasePretty phase => Nat -> List (Statement phase) -> String
  debugStatements _ [] = ""
  debugStatements depth (x :: xs) = debugStatement depth x ++ debugStatements depth xs

  debugStatement : PhasePretty phase => Nat -> Statement phase -> String
  debugStatement depth (MkAstNode info _ statement) = case statement of
    StatementLet binding =>
      node depth "StatementNode" "StatementLet" info "let ...;" ++ debugLet (S depth) binding
    StatementAssignment assignment =>
      node depth "StatementNode" "StatementAssignment" info "<assignment>"
    StatementSemiExpression expression =>
      node depth "StatementNode" "StatementSemiExpression" info (showExprStrict expression ++ ";") ++
      debugExpr (S depth) expression
    StatementExpression expression =>
      node depth "StatementNode" "StatementExpression" info (showExprStrict expression) ++
      debugExpr (S depth) expression

  debugLet : PhasePretty phase => Nat -> LetBindingNode phase -> String
  debugLet depth (MkLetBindingNode _ pattern annotation initializer) =
    indent depth ++ "LetBindingNode MkLetBindingNode\n" ++
    field (S depth) "pattern" (showPattern pattern) ++ debugMaybeTy (S depth) annotation ++
    debugInitializer (S depth) initializer

  debugInitializer : PhasePretty phase => Nat -> Maybe (LetInitializerNode phase) -> String
  debugInitializer _ Nothing = ""
  debugInitializer depth (Just (MkLetInitializerNode marker value)) =
    indent depth ++ "LetInitializerNode MkLetInitializerNode\n" ++
    field (S depth) "marker" (leafText marker) ++ debugExpr (S depth) value

  debugParameter : PhasePretty phase => Nat -> FunctionParameter phase -> String
  debugParameter depth (MkAstNode info _ parameter) = case parameter of
    NormalParameter _ mutability name ty =>
      node depth "FunctionParameterNode" "NormalParameter" info
        (maybe "" (\m => leafText m ++ " ") mutability ++ nameText name ++ ": " ++ showTyStrict ty) ++
      field (S depth) "name" (nameText name) ++ debugTy (S depth) ty
    ReceiverParameter _ borrow =>
      node depth "FunctionParameterNode" "ReceiverParameter" info
        (maybe "self" (\b => leafText b ++ "self") borrow)

  debugParameters : PhasePretty phase => Nat -> List (FunctionParameter phase) -> String
  debugParameters _ [] = ""
  debugParameters depth (x :: xs) = debugParameter depth x ++ debugParameters depth xs

  debugFunction : PhasePretty phase => Nat -> AstInfo -> Item phase -> FunctionDeclarationNode phase -> String
  debugFunction depth info item function = case function of
    MkFunctionDeclarationNode _ _ _ _ _ name params result _ _ body =>
      node depth "ItemNode" "ItemFunction(FunctionDeclarationNode)" info (showItemStrict item) ++
      field (S depth) "functionName" (nameText name) ++ debugParameters (S depth) params ++
      debugMaybeTy (S depth) result ++ debugBlock (S depth) body

  debugItem : PhasePretty phase => Nat -> Item phase -> String
  debugItem depth item@(MkAstNode info _ value) = case value of
    ItemFunction function => debugFunction depth info item function
    ItemModule _ => node depth "ItemNode" "ItemModule" info (showItemStrict item)
    ItemUse _ => node depth "ItemNode" "ItemUse" info (showItemStrict item)
    ItemConst _ => node depth "ItemNode" "ItemConst" info (showItemStrict item)
    ItemEnum _ => node depth "ItemNode" "ItemEnum" info (showItemStrict item)
    ItemQEnum _ => node depth "ItemNode" "ItemQEnum" info (showItemStrict item)
    ItemStruct _ => node depth "ItemNode" "ItemStruct" info (showItemStrict item)
    ItemImpl _ => node depth "ItemNode" "ItemImpl" info (showItemStrict item)

  debugItems : PhasePretty phase => Nat -> List (Item phase) -> String
  debugItems _ [] = ""
  debugItems depth (x :: xs) = debugItem depth x ++ debugItems depth xs

public export
showAstDebug : PhasePretty phase => SourceFile phase -> String
showAstDebug source@(MkAstNode info _ (MkSourceFileNode _ items)) =
  node 0 "SourceFileNode" "MkSourceFileNode" info (showSourceFileStrict source) ++ debugItems 1 items
