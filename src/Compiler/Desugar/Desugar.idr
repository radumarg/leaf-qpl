module Compiler.Desugar.Desugar

import Compiler.Desugar.Helper
import Data.List1
import Frontend.ASTData
import Frontend.ASTPhases
import Frontend.Syntax.Attribute
import Frontend.Syntax.AST
import Frontend.Syntax.Contract
import Frontend.Syntax.Common
import Frontend.Syntax.Doc
import Frontend.Syntax.Literal
import Frontend.Syntax.Name
import Frontend.Syntax.Operator
import Frontend.Syntax.Pattern
import Frontend.Syntax.Type


%default total

desugarAstNode : {a : Type} -> AstNode SurfaceAstPhase a -> AstNode CanonicalAstPhase a
desugarAstNode (MkAstNode docInfo metadata value) = canonicalAstNode docInfo Written value

||| If the attribute is missing argument(s), the name of the function is set as attrribute argument.
||| Unsupported attributes names are ignored, meaning no desugaring is performed for those.
desugarAttribute : String -> Nat -> SurfaceAttribute -> (CanonicalAttribute, Nat)
desugarAttribute defaultArgName nextId (MkAstNode attributeInfo metadata (MkAttributeNode name arguments)) =
  let (desugaredArguments, followingId) = desugarArguments arguments
      attribute = canonicalAstNode attributeInfo Written $
        MkAttributeNode
          (desugarAstNode name)
          desugaredArguments
  in (attribute, followingId)
  where
    argInfo : AstInfo
    argInfo = incrementedAstInfo attributeInfo nextId
    defaultArg : Maybe (List (AttributeArgument CanonicalAstPhase))
    defaultArg = Just [canonicalAstNode argInfo InferredAttributeArgument (AttributeArgumentStringLit ("\"" ++ defaultArgName ++ "\""))]
    desugarArguments : Maybe (List(AttributeArgument SurfaceAstPhase)) -> (Maybe (List (AttributeArgument CanonicalAstPhase)), Nat)
    desugarArguments (Just args) = (Just ((map desugarAstNode) args), nextId)
    desugarArguments Nothing =
      case recognizeKnownAttribute name.value.nameNodeText of
        Just KnownQasmGate => (defaultArg, S nextId)
        Just KnownQasmDef  => (defaultArg, S nextId)
        Nothing            => (Nothing, nextId)

desugarPath : SurfacePath -> CanonicalPath
desugarPath (MkAstNode pathAstInfo metadata (MkPathNode firstSegment remainingSegments)) =
  canonicalAstNode pathAstInfo Written $
    MkPathNode (desugarAstNode firstSegment) (map desugarAstNode remainingSegments)

desugarPattern : SurfacePattern -> CanonicalPattern
desugarPattern (MkAstNode patternInfo _ patternNode) =
  canonicalAstNode patternInfo Written $
    case patternNode of
      PatternWildcard =>
        PatternWildcard
      PatternName mutability binderName =>
        PatternName mutability (desugarAstNode binderName)
      PatternPath valuePath =>
        PatternPath (desugarPath valuePath)
      PatternLiteral literal =>
        PatternLiteral (desugarAstNode literal)
      PatternParenthesized innerPattern =>
        (recur innerPattern).value
      PatternTuple elementPatterns =>
        PatternTuple (map recur elementPatterns)
      PatternArray elementPatterns =>
        PatternArray (map recur elementPatterns)
      PatternStruct structPath fieldPatterns =>
        PatternStruct
          (desugarPath structPath)
          (map desugarStructPatternField fieldPatterns)
      PatternEnumTuple variantPath argumentPatterns =>
        PatternEnumTuple
          (desugarPath variantPath)
          (map recur argumentPatterns)
  where
    recur : SurfacePattern -> CanonicalPattern
    recur pattern =
      desugarPattern (assert_smaller patternNode pattern)

    desugarStructPatternField : SurfaceStructPatternField -> CanonicalStructPatternField
    desugarStructPatternField (MkAstNode fieldInfo _ fieldNode) =
      canonicalAstNode fieldInfo Written $
        case fieldNode of
          StructPatternFieldShorthand mutability fieldAndBinderName =>
            StructPatternFieldShorthand
              mutability
              (desugarAstNode fieldAndBinderName)
          StructPatternFieldExplicit fieldName fieldPattern =>
            StructPatternFieldExplicit
              (desugarAstNode fieldName)
              (recur fieldPattern)

mutual
  desugarExpressionNode : AstInfo -> ExpressionNode SurfaceAstPhase -> ExpressionNode CanonicalAstPhase
  desugarExpressionNode expressionInfo expression =
    case expression of
      ExprLiteral literal => ExprLiteral (desugarAstNode literal)
      ExprName name => ExprName (desugarAstNode name)
      ExprPath path => ExprPath (desugarPath path)
      ExprBuiltin builtin => ExprBuiltin builtin
      ExprSelf => ExprSelf
      ExprParenthesized inner => (desugarNestedExpression inner).value
      ExprTuple elements => ExprTuple (map desugarNestedExpression elements)
      ExprArray elements => ExprArray (map desugarNestedExpression elements)
      ExprRepeatedArray element count => ExprRepeatedArray (desugarNestedExpression element) (desugarNestedExpression count)
      ExprStructLiteral path fields => assert_total $ idris_crash "Desugar.idr: desugarExpressionNode: ExprStructLiteral not implemented"
      ExprCall callee arguments => ExprCall (desugarNestedExpression callee) (map desugarNestedExpression arguments)
      ExprMethodCall receiver name arguments => ExprMethodCall (desugarNestedExpression receiver) (desugarAstNode name) (map desugarNestedExpression arguments)
      ExprField object name => ExprField (desugarNestedExpression object) (desugarAstNode name)
      ExprTupleIndex tuple indexText => ExprTupleIndex (desugarNestedExpression tuple) indexText
      ExprIndex object index => ExprIndex (desugarNestedExpression object) (desugarNestedExpression index)
      ExprUnary operator operand => ExprUnary (desugarAstNode operator) (desugarNestedExpression operand)
      ExprBinary operator left right => ExprBinary (desugarAstNode operator) (desugarNestedExpression left) (desugarNestedExpression right)
      ExprRange start operator end => ExprRange (map desugarNestedExpression start) (desugarAstNode operator) (map desugarNestedExpression end)
      ExprCast operand target => ExprCast (desugarNestedExpression operand) (desugarType target)
      ExprBlock block => ExprBlock (desugarBlockExpression block)
      ExprIf ifNode => ExprIf (desugarIfNode expressionInfo ifNode)
      ExprQIf ifNode => assert_total $ idris_crash "Desugar.idr: desugarExpressionNode: ExprQIf not implemented"
      ExprSIf ifNode => assert_total $ idris_crash "Desugar.idr: desugarExpressionNode: ExprSIf not implemented"
      ExprMatch matchNode => assert_total $ idris_crash "Desugar.idr: desugarExpressionNode: ExprMatch not implemented"
      ExprQMatch matchNode => assert_total $ idris_crash "Desugar.idr: desugarExpressionNode: ExprQMatch not implemented"
      ExprSMatch matchNode => assert_total $ idris_crash "Desugar.idr: desugarExpressionNode: ExprSMatch not implemented"
      ExprLoop body => ExprLoop (desugarBlockExpression body)
      ExprWhile condition body => ExprWhile (desugarNestedExpression condition) (desugarBlockExpression body)
      ExprFor pattern iterator body => ExprFor (desugarPattern pattern) (desugarNestedExpression iterator) (desugarBlockExpression body)
      ExprBreak value => ExprBreak (map desugarNestedExpression value)
      ExprContinue => ExprContinue
      ExprReturn value => ExprReturn (map desugarNestedExpression value)
      ExprCtrl control => ExprCtrl (desugarControlExpressionNode control)
      ExprAdjoint adjoint => ExprAdjoint (desugarAdjointExpressionNode adjoint)
    where
      desugarNestedExpression : SurfaceExpr -> CanonicalExpr
      desugarNestedExpression nestedExpression =
        desugarExpression (assert_smaller expression nestedExpression)
      desugarBlockExpression : SurfaceBlock -> CanonicalBlock
      desugarBlockExpression 
        (MkAstNode blockAstInfo _ (MkBlockNode blockInnerDocs blockStatements finalExpression)) =
        canonicalAstNode blockAstInfo Written $
          MkBlockNode 
            (map desugarAstNode blockInnerDocs) 
            (map (\statement => desugarStatement (assert_smaller expression statement)) blockStatements) 
            (map desugarNestedExpression finalExpression)
      mutual
        -- in case the else statement branch is missing, desugaring adds a default "else { () }" block
        desugarIfNode : AstInfo -> ClassicalIfNode SurfaceAstPhase -> ClassicalIfNode CanonicalAstPhase
        desugarIfNode ifExpressionInfo ifNode@(MkClassicalIfNode ifCondition ifThenBlock Nothing) =
          MkClassicalIfNode
            (desugarNestedExpression ifCondition)
            (desugarBlockExpression ifThenBlock)
            (Just $ ElseBlock (canonicalAstNode desugaredElseBlockAstInfo DefaultElseBlock unitBlockNode))
          where
            desugaredElseBlockAstInfo = incrementedAstInfo ifExpressionInfo 1
            desugaredUnitExpressionAstInfo = incrementedAstInfo ifExpressionInfo 2
            desugaredDefaultUnitValueAstInfo = incrementedAstInfo ifExpressionInfo 3
            unitBlockNode : BlockNode CanonicalAstPhase
            unitBlockNode =
              MkBlockNode [] [] $
                Just (canonicalAstNode 
                  desugaredUnitExpressionAstInfo
                  DesugaredExpression
                  (ExprLiteral $ canonicalAstNode desugaredDefaultUnitValueAstInfo DefaultUnitValue LiteralUnit)
                )
        desugarIfNode _ ifNode@(MkClassicalIfNode ifCondition ifThenBlock ifElseBranch) =
          MkClassicalIfNode
            (desugarNestedExpression ifCondition)
            (desugarBlockExpression ifThenBlock)
            (map (desugarElseNode ifNode) ifElseBranch)
        desugarElseNode : ClassicalIfNode SurfaceAstPhase -> ClassicalElseNode SurfaceAstPhase -> ClassicalElseNode CanonicalAstPhase
        desugarElseNode _ (ElseBlock elseBlock) =
          ElseBlock (desugarBlockExpression elseBlock)
        desugarElseNode ifNode (ElseChainedIf (MkAstNode chainedIfInfo _ chainedIfNode)) =
          ElseChainedIf $
            canonicalAstNode chainedIfInfo Written $
              desugarIfNode chainedIfInfo (assert_smaller ifNode chainedIfNode)
      desugarControlExpressionNode : ControlExpressionNode SurfaceAstPhase -> ControlExpressionNode CanonicalAstPhase
      desugarControlExpressionNode (ControlledCallable controlQubits onBasisRaw controlledCallable) =
        ControlledCallable
          (map desugarNestedExpression controlQubits)
          (map desugarAstNode onBasisRaw)
          (desugarNestedExpression controlledCallable)
      desugarControlExpressionNode (ControlledBlock controlQubits onBasisRaw controlledBlock) =
        ControlledBlock
          (map desugarNestedExpression controlQubits)
          (map desugarAstNode onBasisRaw)
          (desugarBlockExpression controlledBlock)
      desugarAdjointExpressionNode : AdjointExpressionNode SurfaceAstPhase -> AdjointExpressionNode CanonicalAstPhase
      desugarAdjointExpressionNode (AdjointOfCallable adjointedCallable) = AdjointOfCallable (desugarNestedExpression adjointedCallable)
      desugarAdjointExpressionNode (AdjointBlock adjointedBlock) = AdjointBlock (desugarBlockExpression adjointedBlock)

  desugarExpression : SurfaceExpr -> CanonicalExpr
  desugarExpression (MkAstNode expressionInfo metadata expressionNode) =
    canonicalAstNode expressionInfo Written (desugarExpressionNode expressionInfo expressionNode)

  desugarType : Ty SurfaceAstPhase (Expr SurfaceAstPhase) -> Ty CanonicalAstPhase (Expr CanonicalAstPhase)
  desugarType (MkAstNode tyAstInfo metadata typeNode) =
    canonicalAstNode tyAstInfo Written $
      case typeNode of
        TyPrimitive primitiveName =>
          TyPrimitive primitiveName
        TyPath typePath =>
          TyPath (desugarPath typePath)
        TyUnit =>
          TyUnit
        TyParenthesized innerType =>
          (desugarNestedType innerType).value
        TyTuple elementTypes =>
          TyTuple (map desugarNestedType elementTypes)
        TyArray elementType sizeExpression =>
          TyArray 
            (desugarNestedType elementType) 
            (desugarExpression sizeExpression)
        TySlice elementType =>
          TySlice (desugarNestedType elementType)
        TyReference borrowKind referencedType =>
          TyReference 
            (desugarAstNode borrowKind) 
            (desugarNestedType referencedType)
        TyQualified storageQualifiers qualifiedType =>
          TyQualified (map desugarAstNode storageQualifiers) (desugarNestedType qualifiedType)
        TyFunction functionEffect functionParameters returnType =>
          TyFunction
            (map desugarAstNode functionEffect)
            (map desugarParameter functionParameters)
            (map desugarNestedType returnType)
      where
        desugarNestedType : SurfaceTy -> CanonicalTy
        desugarNestedType nestedType =
          desugarType (assert_smaller typeNode nestedType)
        desugarParameter : SurfaceAstNode (FunctionTypeParameterNode SurfaceAstPhase (SurfaceAstNode (ExpressionNode SurfaceAstPhase))) ->
          CanonicalAstNode (FunctionTypeParameterNode CanonicalAstPhase (CanonicalAstNode (ExpressionNode CanonicalAstPhase)))
        desugarParameter (MkAstNode parameterAstInfo metadata (MkFunctionTypeParameterNode parameterName parameterType)) =
          canonicalAstNode parameterAstInfo Written $ 
            MkFunctionTypeParameterNode (desugarAstNode parameterName) (desugarNestedType parameterType)

  desugarFunctionParameter: AstNode SurfaceAstPhase (FunctionParameterNode SurfaceAstPhase) -> AstNode CanonicalAstPhase (FunctionParameterNode CanonicalAstPhase)
  desugarFunctionParameter (MkAstNode parameterInfo metadata (NormalParameter parameterDocs parameterMutability parameterName parameterType)) =
    canonicalAstNode parameterInfo Written $
      NormalParameter
        (map desugarAstNode parameterDocs)
        (map desugarAstNode parameterMutability)
        (desugarAstNode parameterName)
        (desugarType parameterType)
  desugarFunctionParameter (MkAstNode parameterInfo metadata (ReceiverParameter receiverDocs receiverBorrow)) =
    canonicalAstNode parameterInfo Written $
      ReceiverParameter
        (map desugarAstNode receiverDocs)
        (map desugarAstNode receiverBorrow)

  desugarSignedPauliTerm : SurfaceSignedPauliTerm -> SignedPauliTerm CanonicalAstPhase
  desugarSignedPauliTerm (MkAstNode termInfo metadata (MkSignedPauliTermNode sign pauliString)) =
    canonicalAstNode termInfo Written $
      MkSignedPauliTermNode sign (desugarAstNode pauliString)

  desugarContractPredicate : SurfaceContractPredicate -> CanonicalContractPredicate
  desugarContractPredicate (MkAstNode predicateInfo metadata predicateNode) =
    canonicalAstNode predicateInfo Written $
      case predicateNode of
        ContractClean qubitArgument =>
          ContractClean (desugarExpression qubitArgument)
        ContractBasis qubitArgument pauliString =>
          ContractBasis
            (desugarExpression qubitArgument)
            (desugarAstNode pauliString)
        ContractSeparable qubitArgument =>
          ContractSeparable (desugarExpression qubitArgument)
        ContractIsolated qubitArgument =>
          ContractIsolated (desugarExpression qubitArgument)
        ContractProduct firstQubitSet otherQubitSets =>
          ContractProduct
            (desugarExpression firstQubitSet)
            (map desugarExpression otherQubitSets)
        ContractStabilized qubitArgument stabilizerTerms =>
          ContractStabilized
            (desugarExpression qubitArgument)
            (map desugarSignedPauliTerm stabilizerTerms)

  desugarContractClause : ContractClause SurfaceAstPhase (Expr SurfaceAstPhase) -> ContractClause CanonicalAstPhase (Expr CanonicalAstPhase) 
  desugarContractClause (MkAstNode contractAstInfo metadata contractClauseNode) =
    canonicalAstNode contractAstInfo Written $
      case contractClauseNode of
        RequiresClause predicate => RequiresClause (desugarContractPredicate predicate)
        EnsuresClause predicate => EnsuresClause (desugarContractPredicate predicate)

  desugarLetInitializer : LetInitializerNode SurfaceAstPhase -> LetInitializerNode CanonicalAstPhase
  desugarLetInitializer (MkLetInitializerNode marker value) =
    MkLetInitializerNode (desugarAstNode marker) (desugarExpression value)

  desugarAssignmentTarget : SurfaceAstNode (AssignmentTargetNode SurfaceAstPhase) -> CanonicalAstNode (AssignmentTargetNode CanonicalAstPhase)
  desugarAssignmentTarget (MkAstNode assignmentTargetAstInfo metadata assignmentTargetNode) =
    canonicalAstNode assignmentTargetAstInfo Written $
      case assignmentTargetNode of
        AssignTargetName targetName =>
          AssignTargetName (desugarAstNode targetName)
        AssignTargetIndex targetObject indexExpression =>
          AssignTargetIndex
            (desugarExpression targetObject)
            (desugarExpression indexExpression)
        AssignTargetField targetObject fieldName =>
          AssignTargetField
            (desugarExpression targetObject)
            (desugarAstNode fieldName)
        AssignTargetTupleIndex targetObject tupleIndexRawText =>
          AssignTargetTupleIndex
            (desugarExpression targetObject)
            tupleIndexRawText

  ||| the default "linear" qubit qualifier is added if no qubit qualifier is present
  desugarStatement : Statement SurfaceAstPhase -> Statement CanonicalAstPhase
  desugarStatement (MkAstNode statementAstInfo metadata statementNode) =
    canonicalAstNode statementAstInfo Written $
      case statementNode of
        StatementLet (MkLetBindingNode qualifiers pattern typeAnnotation initializer) =>
          StatementLet $
            MkLetBindingNode
              (desugarLetQualifiers typeAnnotation qualifiers)
              (desugarPattern pattern)
              (map (\ty => desugarType (assert_smaller statementNode ty)) typeAnnotation)
              (map (\init => desugarLetInitializer (assert_smaller statementNode init)) initializer)
        StatementAssignment (MkAssignmentNode assignmentTarget assignmentOperator assignmentValue) =>
          desugarAssignmentStatement assignmentTarget assignmentOperator assignmentValue
        StatementSemiExpression statementExpression =>
          StatementSemiExpression (desugarExpression statementExpression)
        StatementExpression statementExpression =>
          StatementExpression (desugarExpression statementExpression)
      where
        inferredLinearQualifier : CanonicalAstNode QuantumStorageQualifier
        inferredLinearQualifier = canonicalAstNode statementAstInfo InferredDefaultQubitQualifier QualifierLinear

        desugarLetQualifiers : Maybe SurfaceTy -> List (AstNode SurfaceAstPhase QuantumStorageQualifier) -> List (AstNode CanonicalAstPhase QuantumStorageQualifier)
        desugarLetQualifiers typeAnnotation [] = if isQubitLikeType typeAnnotation then [inferredLinearQualifier] else []
        desugarLetQualifiers typeAnnotation [scratchQualifier@(MkAstNode _ _ QualifierScratch)] =
          let scratch = desugarAstNode scratchQualifier in
            if isQubitLikeType typeAnnotation then [scratch, inferredLinearQualifier] else [scratch]
        desugarLetQualifiers _ qualifiers = map desugarAstNode qualifiers

        desugarAssignmentStatement : SurfaceAssignmentTarget -> SurfaceAstNode AssignmentOperator -> SurfaceExpr -> StatementNode CanonicalAstPhase
        desugarAssignmentStatement assignmentTarget assignmentOperator@(MkAstNode _ _ AssignValue) assignmentValue =
          StatementAssignment $
            MkAssignmentNode
              (desugarAssignmentTarget assignmentTarget)
              (desugarAstNode assignmentOperator)
              (desugarExpression assignmentValue)
        desugarAssignmentStatement assignmentTarget (MkAstNode astInfo metadata value) assignmentValue =
          StatementAssignment $
            MkAssignmentNode
              (desugarAssignmentTarget assignmentTarget)
              (canonicalAstNode assignmentAstInfo DesugaredAssignment AssignValue)
              (desugarExpression assignmentValue)
            where
              assignmentAstInfo = incrementedAstInfo astInfo 1

desugarFunctionBody : Block SurfaceAstPhase -> Block CanonicalAstPhase
desugarFunctionBody (MkAstNode functionBodyAstInfo metadata (MkBlockNode blockInnerDocs blockStatements finalExpression)) =
  canonicalAstNode functionBodyAstInfo Written $
    MkBlockNode
      (map desugarAstNode blockInnerDocs)
      (map desugarStatement blockStatements)
      (map desugarExpression finalExpression)

desugarItem : SurfaceItem -> CanonicalItem
desugarItem (MkAstNode itemInfo metadata item) =
  canonicalAstNode itemInfo Written $
    case item of
      ItemModule declaration => assert_total $ idris_crash "Desugar.idr: desugarItem: ItemModule not implemented."
      ItemUse declaration => assert_total $ idris_crash "Desugar.idr: desugarItem: ItemUse not implemented."
      ItemConst declaration => ItemConst $ desugarConstDeclaration declaration
      ItemEnum declaration => assert_total $ idris_crash "Desugar.idr: desugarItem: ItemEnum not implemented"
      ItemQEnum declaration => assert_total $ idris_crash "Desugar.idr: desugarItem: ItemQEnum not implemented"
      ItemStruct declaration => assert_total $ idris_crash "Desugar.idr: desugarItem: ItemStruct not implemented"
      ItemImpl declaration => assert_total $ idris_crash "Desugar.idr: desugarItem: ItemImpl not implemented"
      ItemFunction declaration => ItemFunction $ desugarFunctionDeclaration declaration
    where
      desugarConstDeclaration : ConstDeclarationNode SurfaceAstPhase -> ConstDeclarationNode CanonicalAstPhase
      desugarConstDeclaration
          (MkConstDeclarationNode
            constDocs
            constVisibility
            constName
            constType
            constValue) =
              MkConstDeclarationNode
                (map desugarAstNode constDocs)
                (map desugarAstNode constVisibility)
                (desugarAstNode constName)
                (desugarType constType)
                (desugarExpression constValue)
      -- if function effect is missing, desugaring adds the default "general" function effect
      -- if return type is missing, the default "unit" return type is added to function declaration
      -- if function does not return anything, desugaring chnages function body to return the "unit" data
      desugarFunctionDeclaration : FunctionDeclarationNode SurfaceAstPhase -> FunctionDeclarationNode CanonicalAstPhase
      desugarFunctionDeclaration
          (MkFunctionDeclarationNode
            functionDocs
            functionAttributes
            functionVisibility
            functionConstness
            functionEffect
            functionName
            functionParameters
            returnType
            supportClause
            contractClauses
            functionBody
          ) = let
                functionNameString = functionName.value.nameNodeText
                (desugaredAttributes, increment) = mapWithId (desugarAttribute functionNameString) 1 functionAttributes
                (desugaredFunctionEffect, increment) = desugarFunctionEffect functionEffect increment
                desugaredReturnType = desugarFunctionType returnType increment
              in 
                MkFunctionDeclarationNode
                (map desugarAstNode functionDocs)
                desugaredAttributes
                (map desugarAstNode functionVisibility)
                (map desugarAstNode functionConstness)
                desugaredFunctionEffect
                (desugarAstNode functionName)
                (map desugarFunctionParameter functionParameters)
                desugaredReturnType
                (map desugarAstNode supportClause)
                (map desugarContractClause contractClauses)
                (desugarFunctionBody functionBody)
              where
                desugarFunctionEffect : Maybe (AstNode SurfaceAstPhase FunctionEffect) -> Nat -> (Maybe (AstNode CanonicalAstPhase FunctionEffect), Nat)
                desugarFunctionEffect Nothing inc = (Just $ canonicalAstNode (incrementedAstInfo itemInfo inc) InferredDefaultFunctionEffect EffectGeneral, inc + 1)
                desugarFunctionEffect (Just functionEffectNode) inc = (Just $ desugarAstNode functionEffectNode, inc)
                desugarFunctionType : Maybe (Ty SurfaceAstPhase (Expr SurfaceAstPhase)) -> Nat -> Maybe (Ty CanonicalAstPhase (Expr CanonicalAstPhase))
                desugarFunctionType Nothing inc = Just $ canonicalAstNode (incrementedAstInfo itemInfo inc) InferredDefaultFunctionReturnType TyUnit
                desugarFunctionType (Just functionTypeNode) _ = Just $ desugarType functionTypeNode

export
desugarSurfaceSyntax : SurfaceSourceFile -> CanonicalSourceFile
desugarSurfaceSyntax
    (MkAstNode fileInfo metadata (MkSourceFileNode docs items)) =
  canonicalAstNode fileInfo Written $
    MkSourceFileNode
      (map desugarAstNode docs)
      (map desugarItem items)
