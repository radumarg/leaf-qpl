module Desugarer.Helper

import Text.Bounds

import Compiler.Desugar.Desugar
import Frontend.ASTData
import Frontend.ASTPhases
import Frontend.Lexer.Error
import Frontend.Lexer.Lexer
import Frontend.Parser.Error
import Frontend.Parser.Parser
import Frontend.Source
import Frontend.Syntax.AST
import Frontend.Utils.ASTPrettyPrinter
import Frontend.Token

%default total

export
lexParseThenDesugar : String -> String -> Maybe CanonicalSourceFile
lexParseThenDesugar fileName inputProgram =
  case lexFile inputProgram of
    Left _ => Nothing
    Right tokens => case parseFile fileName tokens of
      Left _ => Nothing
      Right surfaceSourceFile => Just (desugarSurfaceSyntax surfaceSourceFile)

export
desugarAndPrettyPrint : String -> Maybe String
desugarAndPrettyPrint inputProgram =
  map showSourceFileStrict (lexParseThenDesugar "test-fixture.rs" inputProgram)
