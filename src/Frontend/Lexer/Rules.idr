module Frontend.Lexer.Rules

import Data.List
import Data.Linear.Ref1
import Data.String
import Derive.Finite
import Syntax.T1
import Text.ILex
import Text.ILex.Interfaces
import Text.ILex.State.Derive
import Text.ILex.State.Regular
import Text.ParseError

import Frontend.Token
import Frontend.Lexer.Error
import Frontend.Lexer.Regex

%default total
%language ElabReflection
%hide Prelude.(>>)
%hide Prelude.(>>=)
%hide Prelude.(<*)
%hide Prelude.pure
%hide Prelude.not

--------------------------------------------------------------------------------
-- A known limitation of ilex: backtrack memory is lost across a node that is
-- extensible but not itself an accept. Concretely: if the lexer has already
-- matched a shorter valid token, then continues into a longer candidate that
-- turns out not to be a real accept anywhere along the way, it hard-fails
-- instead of falling back to the shorter match -- but *only* if some byte in
-- between led to a state with further transitions and no accept of its own.
-- A shorter match followed immediately by a dead end backtracks fine; a
-- shorter match, then one or more such "extend-only" bytes, then a dead end,
-- does not. Worked around below for four distinct grammar shapes; if you hit
-- a case that doesn't fit any of these, it is likely the same underlying
-- bug: read this first rather than removing a workaround that appears
-- redundant.
--
-- Confirmed still present as of collection `nightly-260731` (ilex commit
-- 6c0c590035fdba3a4b6cfaff9dd9deb2791e4aff) by disabling each workaround in
-- turn and reproducing the exact failure described below; first observed
-- under `nightly-260629` (ilex commit fffa455256465705891761a61e37d95c26f0f50e).
--
--   - `digitsThenDotOperatorCandidate` / `emitDigitsThenDotOperator`
--     (this file and `Regex.idr`): without it, `1..2` hard-fails instead
--     of lexing as `1`, `..`, `2`, because the node reached after `1.`
--     extends (a digit could follow) but isn't itself an accept.
--   - `allStarsOuterBlockComment` (`Regex.idr`, wired into `initialRules`
--     below): consumes `/**/`, `/***/`, `/****/`, and so on as complete
--     ordinary comments. The star-run fallback below is already accepting,
--     but would otherwise consume the final `*` as part of the opener and
--     leave the closing `/` stranded in block-comment state.
--   - `bareOuterBlockCommentOpen` (`Regex.idr`, wired into `initialRules`
--     below): accepts the additional-star run used by ordinary Rust
--     banner comments, including the same dead end at true end of input
--     -- e.g. a file truncated right after `/***`. It falls back to an
--     (eventually unterminated) plain block comment, like a bare `/*`.
--   - `unterminatedNormalStringTrailingBackslashCandidate` /
--     `unterminatedByteStringTrailingBackslashCandidate` /
--     `unterminatedByteLiteralTrailingBackslashCandidate` (`Regex.idr`,
--     wired into `initialRules` below): without these, `"abc\`, `b"abc\`,
--     and `b'\` (a string/byte-string/byte-literal body ending in a bare
--     backslash cut off by true end of input) hard-fail instead of
--     falling back to their respective unterminated-literal errors,
--     because the node reached after the lone backslash extends toward a
--     two-character escape but isn't itself an accept. Basis strings
--     have no escape syntax, so they don't share this dead end.
--
-- If you add a new rule whose prefix overlaps an existing shorter rule with
-- a possible dead end in between, test that overlap directly; don't assume
-- maximal munch alone covers it.
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Lexer states.
--
-- The lexer uses exactly two states:
--   * Initial        ordinary Leaf source text
--   * InBlockComment counting state for nested block comments
--
-- Arbitrary nesting is represented by `commentDepth`, not by adding more states.
--------------------------------------------------------------------------------
%runElab deriveParserState "LeafSz" "LeafState" ["Initial", "InBlockComment"]

--------------------------------------------------------------------------------
-- Documentation-comment mode.
--
-- This mode is chosen by the outermost block-comment opener. Nested block
-- comments only affect the extra nesting depth; they never change the doc/non-doc mode
-- of the outer comment.
--------------------------------------------------------------------------------
data CommentMode
  = NormalBlockComment
  | OuterBlockDocComment
  | InnerBlockDocComment

commentModeToToken : CommentMode -> String -> Maybe Token
commentModeToToken NormalBlockComment _ = Nothing
commentModeToToken OuterBlockDocComment rawText = Just (TokOuterDoc rawText)
commentModeToToken InnerBlockDocComment rawText = Just (TokInnerDoc rawText)

--------------------------------------------------------------------------------
-- Mutable ilex state.
--
-- The installed Text.ILex.State.Regular.State record already supplies HasBytes,
-- HasStringLits, HasBBErr, and HasStack instances (it derives them via
-- `%runElab derive "State" [FullState]`). `LeafStack` only has to carry what
-- that generic record does not already provide: the emitted token buffer and
-- block-comment depth/mode counters. `commentDepth` counts nested comments
-- beyond the outermost block comment while in `InBlockComment`.
--------------------------------------------------------------------------------
record LeafStack where
  constructor MkLeafStack
  outputTokens : SnocList (ByteBounded Token)
  commentDepth : Nat
  commentMode  : CommentMode

0 LeafLexerStack : Type -> Type
LeafLexerStack = State LexerError LeafStack LeafSz

initLeafStack : LeafStack
initLeafStack = MkLeafStack [<] Z NormalBlockComment

--------------------------------------------------------------------------------
-- Local rule alias.
--------------------------------------------------------------------------------
0 LeafRule : Type -> Type
LeafRule q = (RExp True, Step q LeafSz LeafLexerStack)

--------------------------------------------------------------------------------
-- Bounds and position helpers built on top of the installed Text.ILex
-- position/error machinery (`Text.ILex.Interfaces`).
--------------------------------------------------------------------------------
oldestOpenPosition : SnocList BytePos -> Maybe BytePos
oldestOpenPosition [<] =
  Nothing
oldestOpenPosition (olderPositions :< openPosition) =
  case olderPositions of
    [<] =>
      Just openPosition

    _ =>
      oldestOpenPosition olderPositions

unterminatedCommentBounds :
     (sk : LeafLexerStack q)
  => BytePos
  -> F1 q ByteBounds
unterminatedCommentBounds openPosition = T1.do
  endPosition <- endPos
  pure (BB openPosition endPosition)

--------------------------------------------------------------------------------
-- Suffix-stripping helper, used by `classifyDotOperatorSuffix` below to split
-- the `.`/`..`/`..=` operator off the trailing end of a matched
-- `digitsThenDotOperatorCandidate`.
--------------------------------------------------------------------------------
stripReversedSuffix : List Char -> List Char -> Maybe (List Char)
stripReversedSuffix [] remainingReversedChars =
  Just (reverse remainingReversedChars)
stripReversedSuffix (suffixChar :: suffixRest) (valueChar :: valueRest) =
  case suffixChar == valueChar of
    True  => stripReversedSuffix suffixRest valueRest
    False => Nothing
stripReversedSuffix _ [] = Nothing

stripSuffixChars : List Char -> List Char -> Maybe (List Char)
stripSuffixChars suffixChars valueChars =
  stripReversedSuffix (reverse suffixChars) (reverse valueChars)

--------------------------------------------------------------------------------
-- Literal validation.
--
-- Rather than re-parsing the already-matched raw text by hand, run it back
-- through the *strict* regexes from `Frontend.Lexer.Regex` (`integerLiteral`,
-- `floatLiteral`, `normalStringLiteralStrict`, ...) via
-- `Text.ILex.State.Regular.value`, ilex's own primitive for classifying a whole
-- string against a set of regex
-- alternatives. Its "done" state has an empty DFA for anything but `Ignore`
-- alternatives, so any leftover unconsumed input after the first match fails
-- outright -- this rejects partial/prefix matches for free, with no
-- hand-rolled slicing, and keeps the strict regexes as the single source of
-- truth for what counts as a well-formed literal instead of a second,
-- independently-maintained copy of the same grammar.
--------------------------------------------------------------------------------
runValueMatch : Parser1 (BBErr Void) a -> String -> Maybe a
runValueMatch parser text =
  case runString parser text of
    Right result => Just result
    Left _        => Nothing

matchesLiteral : Parser1 (BBErr Void) () -> String -> Bool
matchesLiteral parser text =
  case runValueMatch parser text of
    Just () => True
    Nothing => False

data NumberLiteralKind
  = IntegerNumberLiteral
  | FloatingNumberLiteral

numberClassifier : PVal1 q Void NumberLiteralKind
numberClassifier =
  value Nothing
    [ (integerLiteral, const IntegerNumberLiteral)
    , (floatLiteral,   const FloatingNumberLiteral)
    ]

classifyNumberLiteral : String -> Maybe NumberLiteralKind
classifyNumberLiteral = runValueMatch numberClassifier

normalStringValidator : PVal1 q Void ()
normalStringValidator =
  value Nothing [(normalStringLiteralStrict, const ())]

validNormalStringLiteral : String -> Bool
validNormalStringLiteral = matchesLiteral normalStringValidator

basisStringValidator : PVal1 q Void ()
basisStringValidator =
  value Nothing [(basisStringLiteralStrict, const ())]

validBasisStringLiteral : String -> Bool
validBasisStringLiteral = matchesLiteral basisStringValidator

byteLiteralValidator : PVal1 q Void ()
byteLiteralValidator =
  value Nothing [(byteLiteralStrict, const ())]

validByteLiteral : String -> Bool
validByteLiteral = matchesLiteral byteLiteralValidator

byteStringValidator : PVal1 q Void ()
byteStringValidator =
  value Nothing [(byteStringLiteralStrict, const ())]

validByteStringLiteral : String -> Bool
validByteStringLiteral = matchesLiteral byteStringValidator

--------------------------------------------------------------------------------
-- Splits the dot-operator suffix matched by `digitsThenDotOperatorCandidate`
-- (one of `.`, `..`, or `..=`) off the trailing end of the matched text,
-- returning the leading digits together with the matched `Symbol`.
--------------------------------------------------------------------------------
classifyDotOperatorSuffix : List Char -> (List Char, Symbol)
classifyDotOperatorSuffix chars =
  case stripSuffixChars (unpack "..=") chars of
    Just digitChars => (digitChars, SymDotDotEq)
    Nothing =>
      case stripSuffixChars (unpack "..") chars of
        Just digitChars => (digitChars, SymDotDot)
        Nothing =>
          case stripSuffixChars (unpack ".") chars of
            Just digitChars => (digitChars, SymDot)
            Nothing => (chars, SymDot)

--------------------------------------------------------------------------------
-- Token and error actions.
--------------------------------------------------------------------------------
emitBoundedToken :
     (sk : LeafLexerStack q)
  => ByteBounded Token
  -> F1 q LeafState
emitBoundedToken boundedToken = T1.do
  st <- getStack
  putStackAs ({ outputTokens $= (:< boundedToken) } st) Initial

emitToken :
     (sk : LeafLexerStack q)
  => Token
  -> F1 q LeafState
emitToken token = T1.do
  boundedToken <- bounded' token
  emitBoundedToken boundedToken

||| Remembers the first fatal error only. `failHere` itself now keeps the
||| first error and ignores later ones (ilex commit ee97414 onward), so this
||| is just the domain-named entry point Rules.idr's call sites use.
rememberFatalError :
     (sk : LeafLexerStack q)
  => LexerError
  -> F1 q LeafState
rememberFatalError lexerError =
  failHere (Custom lexerError) Initial

||| As `rememberFatalError`, but at an explicit span rather than the current
||| position.
rememberFatalErrorAt :
     (sk : LeafLexerStack q)
  => ByteBounds
  -> LexerError
  -> F1 q LeafState
rememberFatalErrorAt errorBounds lexerError =
  failWith (B (Custom lexerError) errorBounds) Initial

emitValidatedLiteral :
     (sk : LeafLexerStack q)
  => (String -> Bool)
  -> (String -> LexerError)
  -> (String -> Token)
  -> String
  -> F1 q LeafState
emitValidatedLiteral validator errorBuilder tokenBuilder rawText =
  case validator rawText of
    True => emitToken (tokenBuilder rawText)
    False => rememberFatalError (errorBuilder rawText)

emitNormalStringLiteral :
     (sk : LeafLexerStack q)
  => String
  -> F1 q LeafState
emitNormalStringLiteral =
  emitValidatedLiteral
    validNormalStringLiteral
    LexInvalidStringLiteral
    TokStringLitRaw

emitBasisStringLiteral :
     (sk : LeafLexerStack q)
  => String
  -> F1 q LeafState
emitBasisStringLiteral =
  emitValidatedLiteral
    validBasisStringLiteral
    LexInvalidBasisStringLiteral
    TokBasisStringLitRaw

emitByteLiteral :
     (sk : LeafLexerStack q)
  => String
  -> F1 q LeafState
emitByteLiteral =
  emitValidatedLiteral
    validByteLiteral
    LexInvalidByteLiteral
    TokByteLitRaw

emitByteStringLiteral :
     (sk : LeafLexerStack q)
  => String
  -> F1 q LeafState
emitByteStringLiteral =
  emitValidatedLiteral
    validByteStringLiteral
    LexInvalidByteStringLiteral
    TokByteStringLitRaw

emitNumberLiteral :
     (sk : LeafLexerStack q)
  => String
  -> F1 q LeafState
emitNumberLiteral rawText =
  case classifyNumberLiteral rawText of
    Just IntegerNumberLiteral =>
      emitToken (TokIntLitRaw rawText)

    Just FloatingNumberLiteral =>
      emitToken (TokFloatLitRaw rawText)

    Nothing =>
      rememberFatalError (LexInvalidNumberLiteral rawText)

emitDigitsThenDotOperator :
     (sk : LeafLexerStack q)
  => String
  -> F1 q LeafState
emitDigitsThenDotOperator rawText = T1.do
  let (digitChars, dotSymbol) = classifyDotOperatorSuffix (unpack rawText)
  let digitText = pack digitChars
  case classifyNumberLiteral digitText of
    Just IntegerNumberLiteral => T1.do
      let digitsLength = length digitChars
      tokenStart <- startPos
      tokenEnd <- endPos
      let integerEnd = incLen digitsLength tokenStart
      let operatorStart = incLen (S digitsLength) tokenStart
      _ <- emitBoundedToken (B (TokIntLitRaw digitText) (BB tokenStart integerEnd))
      emitBoundedToken (B (TokSym dotSymbol) (BB operatorStart tokenEnd))

    _ => T1.do
      tokenStart <- startPos
      let integerEnd = incLen (length digitChars) tokenStart
      rememberFatalErrorAt
        (BB tokenStart integerEnd)
        (LexInvalidNumberLiteral digitText)

--------------------------------------------------------------------------------
-- Block-comment actions.
--
-- The block-comment depth and mode live on `LeafStack`; doc-comment text
-- pieces are accumulated with ilex's built-in string-literal accumulator
-- (`pushStr'`/`getStr`).
--------------------------------------------------------------------------------
appendTextIfDoc :
     (sk : LeafLexerStack q)
  => CommentMode
  -> String
  -> F1' q
appendTextIfDoc NormalBlockComment _ = pure ()
appendTextIfDoc OuterBlockDocComment rawText = pushStr' rawText
appendTextIfDoc InnerBlockDocComment rawText = pushStr' rawText

beginBlockComment :
     (sk : LeafLexerStack q)
  => CommentMode
  -> String
  -> F1 q LeafState
beginBlockComment mode rawText = T1.do
  pushPosition
  st <- getStack
  putStack ({ commentDepth := Z, commentMode := mode } st)
  appendTextIfDoc mode rawText
  pure InBlockComment

beginNestedBlockComment :
     (sk : LeafLexerStack q)
  => String
  -> F1 q LeafState
beginNestedBlockComment rawText = T1.do
  pushPosition
  st <- getStack
  putStack ({ commentDepth $= S } st)
  appendTextIfDoc st.commentMode rawText
  pure InBlockComment

finishOutermostBlockComment :
     (sk : LeafLexerStack q)
  => CommentMode
  -> ByteBounds
  -> F1 q LeafState
finishOutermostBlockComment mode fullCommentBounds = T1.do
  rawCommentText <- getStr
  case commentModeToToken mode rawCommentText of
    Nothing => pure Initial
    Just docToken =>
      emitBoundedToken (B docToken fullCommentBounds)

closeBlockComment :
     (sk : LeafLexerStack q)
  => ByteBounds
  -> F1 q LeafState
closeBlockComment fullCommentBounds = T1.do
  st <- getStack
  appendTextIfDoc st.commentMode "*/"
  case st.commentDepth of
    Z =>
      finishOutermostBlockComment st.commentMode fullCommentBounds

    S remainingDepth => T1.do
      putStack ({ commentDepth := remainingDepth } st)
      pure InBlockComment

consumeBlockCommentText :
     (sk : LeafLexerStack q)
  => String
  -> F1 q LeafState
consumeBlockCommentText rawText = T1.do
  st <- getStack
  appendTextIfDoc st.commentMode rawText
  pure InBlockComment

symbolRules : List (LeafRule q)
symbolRules =
  vals
    showSymbolLeaf
    (\symbol, stackValue => emitToken {sk = stackValue} (TokSym symbol))
    values

--------------------------------------------------------------------------------
-- Initial-state rules.
--
-- Overlapping rules are ordered intentionally; ilex still uses maximal munch,
-- so order matters only when alternatives match the same longest prefix:
--   1. documentation comments before ordinary comments and before `/`
--   2. broad literal candidates before identifiers
--   3. broad number candidate before identifier-like suffixes
--   4. single identifier rule through `tokenFromIdentLike`
--   5. generated symbol rules from the finite `Symbol` values
--------------------------------------------------------------------------------
initialRules : List (LeafRule q)
initialRules =
  [ string outerDocLineComment (\rawText => emitToken (TokOuterDoc rawText))
  , string innerDocLineComment (\rawText => emitToken (TokInnerDoc rawText))

  , ignore allStarsOuterBlockComment
  , string outerBlockDocOpen (beginBlockComment OuterBlockDocComment)
  , string innerBlockDocOpen (beginBlockComment InnerBlockDocComment)
  , string bareOuterBlockCommentOpen (beginBlockComment NormalBlockComment)
  , ignore normalLineComment
  , string normalBlockCommentOpen (beginBlockComment NormalBlockComment)
  , ignore leafWhitespace

  , string basisStringCandidate emitBasisStringLiteral
  , string byteStringCandidate emitByteStringLiteral
  , string byteLiteralCandidate emitByteLiteral
  , string normalStringCandidate emitNormalStringLiteral

  -- Unterminated candidates come after closed-literal candidates, so a valid
  -- string wins by maximal munch. They come before identifiers so `bs"bad` is
  -- not split into `bs` and a string fragment.
  , step unterminatedBasisStringCandidate
      (rememberFatalError LexUnterminatedBasisStringLiteral)
  , step unterminatedByteStringCandidate
      (rememberFatalError LexUnterminatedByteStringLiteral)
  , step unterminatedByteLiteralCandidate
      (rememberFatalError LexUnterminatedByteLiteral)
  , step unterminatedNormalStringCandidate
      (rememberFatalError LexUnterminatedStringLiteral)

  -- Trailing-backslash sub-cases of the three candidates just above (see the
  -- known-limitation note at the top of this file): only ever the longest
  -- match when the escape is cut off by true end of input, since maximal
  -- munch prefers the longer complete-escape match otherwise.
  , step unterminatedByteStringTrailingBackslashCandidate
      (rememberFatalError LexUnterminatedByteStringLiteral)
  , step unterminatedByteLiteralTrailingBackslashCandidate
      (rememberFatalError LexUnterminatedByteLiteral)
  , step unterminatedNormalStringTrailingBackslashCandidate
      (rememberFatalError LexUnterminatedStringLiteral)
  , step ordinaryCharLiteralCandidate
      (rememberFatalError LexOrdinaryCharLiteralNeedsToken)

  , string numberCandidate emitNumberLiteral
  , string digitsThenDotOperatorCandidate emitDigitsThenDotOperator
  , string identifierLike (\rawText => emitToken (tokenFromIdentLike rawText))
  ] ++ symbolRules

--------------------------------------------------------------------------------
-- Block-comment rules.
--
-- Active only in `InBlockComment`. `/*` increments depth, `*/` decrements depth,
-- and only the outermost close returns to Initial.
--------------------------------------------------------------------------------
blockCommentRules : List (LeafRule q)
blockCommentRules =
  [ string normalBlockCommentOpen beginNestedBlockComment
  , closeWithBounds blockCommentClose closeBlockComment
  , string blockCommentBodyChunk consumeBlockCommentText
  , string blockCommentLineBreak consumeBlockCommentText
  , string blockCommentSingleStar consumeBlockCommentText
  , string blockCommentSingleSlash consumeBlockCommentText
  ]

--------------------------------------------------------------------------------
-- DFAs, error handlers, and final P1 lexer.
--------------------------------------------------------------------------------
leafLexerSteps : Lex1 q LeafSz LeafLexerStack
leafLexerSteps =
  lex1
    [ E Initial (dfa initialRules)
    , E InBlockComment (dfa blockCommentRules)
    ]

leafLexerEOI :
     LeafState
  -> LeafLexerStack q
  -> F1 q (Either (BBErr LexerError) (List (ByteBounded Token)))
leafLexerEOI lexerState stackValue = T1.do
  storedError <- read1 (error stackValue)
  case storedError of
    Just existingError =>
      pure (Left existingError)

    Nothing =>
      case lexerState == Initial of
        True => T1.do
          st <- read1 (stack stackValue)
          eofPosition <- endPos
          pure (Right (st.outputTokens <>> [B TokEOF (BB eofPosition eofPosition)]))

        False => T1.do
          openPositions <- read1 (positions stackValue)
          case oldestOpenPosition openPositions of
            Just openPosition => T1.do
              unclosedBounds <- unterminatedCommentBounds openPosition
              pure (Left (B (Custom LexUnterminatedBlockComment) unclosedBounds))

            Nothing => T1.do
              eofPosition <- endPos
              pure
                (Left
                  (B
                    (Custom
                      (LexInternalLexerError
                        "InBlockComment at end of input without an opening position"))
                    (BB eofPosition eofPosition)))

leafLexer : Lexer LexerError Token
leafLexer =
  P Initial
    (init initLeafStack)
    leafLexerSteps
    noChunk
    (errs [])
    leafLexerEOI

export
runLeafLexer : String -> Either (BBErr LexerError) (List (ByteBounded Token))
runLeafLexer = runString leafLexer
