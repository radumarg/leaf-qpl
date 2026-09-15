module Frontend.Lexer.Regex

import Text.ILex
import Text.ILex.RExp.Unicode as Uni

%default total
%hide Prelude.(>>)
%hide Prelude.not

--------------------------------------------------------------------------------
-- Whitespace and line structure.
--
-- Leaf accepts CRLF, LF, and bare CR as line breaks, and spaces and tabs as
-- horizontal whitespace.
--------------------------------------------------------------------------------
lineBreak : RExp True
lineBreak =
      "\r\n"
  <|> '\n'
  <|> '\r'

horizontalWhitespace : RExp True
horizontalWhitespace =
  oneof [' ', '\t']

export
leafWhitespace : RExp True
leafWhitespace =
  plus (horizontalWhitespace <|> lineBreak)

-- In ilex, `dot` means a printable (non-control) code point, not an arbitrary
-- character. Comments and broad literal candidates must also consume tabs and
-- other control code points so that they cannot terminate a match prematurely.
-- `range32` intersects this range with Unicode's valid scalar values when the
-- DFA is generated.
validCodePoint : RExp True
validCodePoint =
  range32 0x0 0x10ffff

notLineBreakChar : RExp True
notLineBreakChar =
  validCodePoint && not '\n' && not '\r'

--------------------------------------------------------------------------------
-- ASCII helpers used by string and number candidates.
--------------------------------------------------------------------------------
asciiLower : RExp True
asciiLower = range 'a' 'z'

asciiUpper : RExp True
asciiUpper = range 'A' 'Z'

asciiLetter : RExp True
asciiLetter = asciiLower <|> asciiUpper

asciiAlphaNum : RExp True
asciiAlphaNum = asciiLetter <|> digit

asciiAlphaNumUnderscore : RExp True
asciiAlphaNumUnderscore = asciiAlphaNum <|> '_'

--------------------------------------------------------------------------------
-- Identifier-like text.
--
-- There is intentionally one identifier rule.  `tokenFromIdentLike` in
-- Frontend.Token owns the classification into TokUnderscore, booleans,
-- keywords, primitive types, state literals, builtins, and ordinary identifiers.
--
-- Leaf identifier syntax is:
--   start = Unicode alphabetic or '_'
--   rest  = Unicode alphabetic, Unicode digit, '_', or apostrophe
--
-- Therefore `foo'` is a single identifier and `fn'` is not split into `fn` plus
-- an apostrophe.
--
-- `Text.ILex`'s own `alpha`/`alphaNum` are ASCII-only (`a`-`z`/`A`-`Z`/`0`-`9`),
-- despite reading as generic names, so they cannot be used here. Genuine
-- Unicode letters and digits come from `Text.ILex.RExp.Unicode`'s generated
-- Unicode general-category tables instead: `letter` is categories
-- Lu+Ll+Lt+Lm+Lo ("Unicode alphabetic") and `decimalNumber` is category Nd
-- ("Unicode digit").
--------------------------------------------------------------------------------
identifierStart : RExp True
identifierStart = Uni.letter <|> '_'

identifierRest : RExp True
identifierRest = Uni.letter <|> Uni.decimalNumber <|> '_' <|> '\''

export
identifierLike : RExp True
identifierLike = identifierStart >> star identifierRest

--------------------------------------------------------------------------------
-- Numeric literal components.
--
-- Unary minus is deliberately not included here.  `-7` is lexed as the symbol
-- `-` followed by `TokIntLitRaw "7"`.
--------------------------------------------------------------------------------
decimalDigits : RExp True
decimalDigits = digit >> star (digit <|> '_')

--------------------------------------------------------------------------------
-- Strict digit runs, used only for validating an already-matched literal (see
-- `Rules.idr`'s `numberClassifier`), never for the broad candidates above.
--
-- Interior `_` is freely placed (including consecutive underscores), but a
-- decimal digit run may not start or end with `_`, while a radix digit run
-- (used after `0b`/`0o`/`0x`) may start with `_` (Rust-style `0x_FF`) but still
-- may not end with one. The permissive `decimalDigits` above must stay
-- permissive so malformed spellings are swallowed into one token instead of
-- being split, so this is a separate definition rather than a tightened
-- version of it. Radix candidates (`radixNumberCandidate` below) don't have an
-- equivalent permissive per-radix helper: they swallow any alphanumeric run
-- after the `0b`/`0o`/`0x` prefix directly, so an out-of-range digit like the
-- `2` in `0b102` still ends up in one malformed-literal token instead of
-- being split off.
--------------------------------------------------------------------------------
strictDigitRun : RExp True -> RExp True
strictDigitRun singleDigit =
  singleDigit >> opt (star (singleDigit <|> '_') >> singleDigit)

strictRadixDigitRun : RExp True -> RExp True
strictRadixDigitRun singleDigit =
  star (singleDigit <|> '_') >> singleDigit

strictDecimalDigits : RExp True
strictDecimalDigits = strictDigitRun digit

strictBinaryDigits : RExp True
strictBinaryDigits = strictRadixDigitRun bindigit

strictOctalDigits : RExp True
strictOctalDigits = strictRadixDigitRun octdigit

strictHexDigits : RExp True
strictHexDigits = strictRadixDigitRun hexdigit

signedIntegerTypeSuffix : RExp True
signedIntegerTypeSuffix =
      "i8"
  <|> "i16"
  <|> "i32"
  <|> "i64"
  <|> "i128"

unsignedIntegerTypeSuffix : RExp True
unsignedIntegerTypeSuffix =
      "u8"
  <|> "u16"
  <|> "u32"
  <|> "u64"
  <|> "u128"

integerTypeSuffix : RExp True
integerTypeSuffix = signedIntegerTypeSuffix <|> unsignedIntegerTypeSuffix

integerSuffix : RExp True
integerSuffix = opt '_' >> integerTypeSuffix

floatTypeSuffix : RExp True
floatTypeSuffix = "f32" <|> "f64"

floatSuffix : RExp True
floatSuffix = opt '_' >> floatTypeSuffix

exponentPart : RExp True
exponentPart =
  oneof ['e', 'E'] >> opt (oneof ['+', '-']) >> strictDecimalDigits

--------------------------------------------------------------------------------
-- Valid numeric regexes.
--
-- These are the actual source of truth for what counts as a well-formed
-- integer/float literal: `Rules.idr`'s `numberClassifier` runs the already
-- broadly-matched raw text back through these via
-- `Text.ILex.State.Regular.value`,
-- rather than re-validating it with hand-written `List Char` recursion.
--------------------------------------------------------------------------------
binaryIntegerLiteral : RExp True
binaryIntegerLiteral =
  ("0b" <|> "0B") >> strictBinaryDigits >> opt integerSuffix

octalIntegerLiteral : RExp True
octalIntegerLiteral =
  ("0o" <|> "0O") >> strictOctalDigits >> opt integerSuffix

hexIntegerLiteral : RExp True
hexIntegerLiteral =
  ("0x" <|> "0X") >> strictHexDigits >> opt integerSuffix

decimalIntegerLiteral : RExp True
decimalIntegerLiteral =
  strictDecimalDigits >> opt integerSuffix

export
integerLiteral : RExp True
integerLiteral =
      binaryIntegerLiteral
  <|> octalIntegerLiteral
  <|> hexIntegerLiteral
  <|> decimalIntegerLiteral

--------------------------------------------------------------------------------
-- Supported float forms:
--
--   digit+ '.' digit+ exp? suf?
--   digit+ exp suf?
--   digit+ ('f32' | 'f64')
--
-- The rule does not admit trailing-dot floats.  This also preserves the required
-- tokenization of `1..2` as integer, range operator, integer.
--------------------------------------------------------------------------------
export
floatLiteral : RExp True
floatLiteral =
      (strictDecimalDigits >> '.' >> strictDecimalDigits >> opt exponentPart >> opt floatSuffix)
  <|> (strictDecimalDigits >> exponentPart >> opt floatSuffix)
  <|> (strictDecimalDigits >> floatSuffix)

--------------------------------------------------------------------------------
-- Broad numeric candidates.
--
-- The lexer must not split malformed numeric spellings into smaller legal
-- tokens.  For example, `0b102` must become a number-literal error rather than
-- `0b10` followed by `2`.  These candidates therefore consume complete
-- number-looking spans, and Rules.idr validates/classifies the raw spelling.
--
-- We intentionally do not consume a bare trailing dot.  That keeps `1..2`
-- correct and leaves `1.` as `1` followed by `.`, which the parser can reject if
-- it appears in a context where a member-access dot is not meaningful.
--------------------------------------------------------------------------------
radixNumberCandidate : RExp True
radixNumberCandidate =
      (("0b" <|> "0B") >> star (asciiAlphaNum <|> '_'))
  <|> (("0o" <|> "0O") >> star (asciiAlphaNum <|> '_'))
  <|> (("0x" <|> "0X") >> star (asciiAlphaNum <|> '_'))

looseExponentPart : RExp True
looseExponentPart =
  oneof ['e', 'E'] >> opt (oneof ['+', '-']) >> star (asciiAlphaNum <|> '_')

dottedDecimalNumberCandidate : RExp True
dottedDecimalNumberCandidate =
  decimalDigits >> '.' >> decimalDigits >> opt looseExponentPart >> star (asciiAlphaNum <|> '_')

exponentNumberCandidate : RExp True
exponentNumberCandidate =
  decimalDigits >> looseExponentPart >> star (asciiAlphaNum <|> '_')

suffixedDecimalNumberCandidate : RExp True
suffixedDecimalNumberCandidate =
  decimalDigits >> (asciiLetter <|> '_') >> star (asciiAlphaNum <|> '_')

plainDecimalNumberCandidate : RExp True
plainDecimalNumberCandidate = decimalDigits

export
numberCandidate : RExp True
numberCandidate =
      dottedDecimalNumberCandidate
  <|> radixNumberCandidate
  <|> exponentNumberCandidate
  <|> suffixedDecimalNumberCandidate
  <|> plainDecimalNumberCandidate

--------------------------------------------------------------------------------
-- Digits immediately followed by `.`, `..`, or `..=`.
--
-- This regex also matches the prefix of a dotted float such as `1.2`, but
-- `dottedDecimalNumberCandidate` consumes the longer span and wins by ilex's
-- maximal-munch rule. When no digit follows the first dot, this candidate wins
-- instead and the action splits the match into an integer plus `.`, `..`, or
-- `..=`; thus `1..2` lexes as `1`, `..`, `2` and `1.` as `1`, `.`.
--------------------------------------------------------------------------------
export
digitsThenDotOperatorCandidate : RExp True
digitsThenDotOperatorCandidate =
  decimalDigits >> ('.' >> opt ('.' >> opt '='))

--------------------------------------------------------------------------------
-- String, basis-string, byte literal, byte-string, and ordinary char candidates.
--
-- These candidates are deliberately broader than the set of valid literals.
-- Rules.idr validates the raw spelling and raises a structured error instead of
-- letting bad literals split into unrelated tokens.
--------------------------------------------------------------------------------
normalStringBodyCandidate : RExp True
normalStringBodyCandidate =
      ('\\' >> notLineBreakChar)
  <|> (notLineBreakChar && not '"' && not '\\')

export
normalStringCandidate : RExp True
normalStringCandidate =
  '"' >> star normalStringBodyCandidate >> '"'

export
unterminatedNormalStringCandidate : RExp True
unterminatedNormalStringCandidate =
  '"' >> star normalStringBodyCandidate

-- Covers a body ending in a bare backslash cut off by true end of input,
-- before the escape's second character ever arrives -- the same class of
-- backtracking dead end as `bareOuterBlockCommentOpen` below, just for an
-- escape-introducing backslash instead of a doc-comment star run. Without
-- this, `"abc\` hard-fails instead of falling back to an unterminated
-- string, because the node reached after the lone backslash extends toward
-- a two-character escape but isn't itself an accept.
export
unterminatedNormalStringTrailingBackslashCandidate : RExp True
unterminatedNormalStringTrailingBackslashCandidate =
  '"' >> star normalStringBodyCandidate >> '\\'

export
basisStringCandidate : RExp True
basisStringCandidate =
  'b' >> 's' >> '"' >> star (notLineBreakChar && not '"') >> '"'

export
unterminatedBasisStringCandidate : RExp True
unterminatedBasisStringCandidate =
  'b' >> 's' >> '"' >> star (notLineBreakChar && not '"')

-- Byte strings allow the same body characters as normal strings (anything
-- but '"', a line break, or a bare backslash); the two candidates share one
-- definition so they can't silently drift apart.
byteStringBodyCandidate : RExp True
byteStringBodyCandidate = normalStringBodyCandidate

export
byteStringCandidate : RExp True
byteStringCandidate =
  'b' >> '"' >> star byteStringBodyCandidate >> '"'

export
unterminatedByteStringCandidate : RExp True
unterminatedByteStringCandidate =
  'b' >> '"' >> star byteStringBodyCandidate

-- Same dead end as `unterminatedNormalStringTrailingBackslashCandidate`
-- above, for byte strings: `b"abc\` at true end of input.
export
unterminatedByteStringTrailingBackslashCandidate : RExp True
unterminatedByteStringTrailingBackslashCandidate =
  'b' >> '"' >> star byteStringBodyCandidate >> '\\'

byteLiteralBodyCandidate : RExp True
byteLiteralBodyCandidate =
      ('\\' >> notLineBreakChar)
  <|> (notLineBreakChar && not '\'' && not '\\')

export
byteLiteralCandidate : RExp True
byteLiteralCandidate =
  'b' >> '\'' >> star byteLiteralBodyCandidate >> '\''

export
unterminatedByteLiteralCandidate : RExp True
unterminatedByteLiteralCandidate =
  'b' >> '\'' >> star byteLiteralBodyCandidate

-- Same dead end as `unterminatedNormalStringTrailingBackslashCandidate`
-- above, for byte literals: `b'\` at true end of input.
export
unterminatedByteLiteralTrailingBackslashCandidate : RExp True
unterminatedByteLiteralTrailingBackslashCandidate =
  'b' >> '\'' >> star byteLiteralBodyCandidate >> '\\'

export
ordinaryCharLiteralCandidate : RExp True
ordinaryCharLiteralCandidate =
  '\'' >> star byteLiteralBodyCandidate >> '\''

--------------------------------------------------------------------------------
-- Strict string/byte-literal regexes, used only for validating the raw text
-- already matched by the broad candidates above (see `Rules.idr`'s
-- `normalStringValidator`/`basisStringValidator`/`byteLiteralValidator`/
-- `byteStringValidator`), never for matching directly.
--------------------------------------------------------------------------------
export
normalStringLiteralStrict : RExp True
normalStringLiteralStrict =
  '"' >> star asciiAlphaNumUnderscore >> '"'

basisStringChar : RExp True
basisStringChar =
  oneof ['0', '1', '+', '-', 'i', 'I']

export
basisStringLiteralStrict : RExp True
basisStringLiteralStrict =
  'b' >> 's' >> '"' >> star basisStringChar >> '"'

simpleByteEscape : RExp True
simpleByteEscape =
  '\\' >> oneof ['n', 'r', 't', '0', '\\', '\'', '"']

hexByteEscape : RExp True
hexByteEscape =
  "\\x" >> hexdigit >> hexdigit

export
byteLiteralStrict : RExp True
byteLiteralStrict =
  'b' >> '\'' >>
    ((range32 0x20 0x7e && not '\\' && not '\'') <|> simpleByteEscape <|> hexByteEscape) >>
    '\''

export
byteStringLiteralStrict : RExp True
byteStringLiteralStrict =
  'b' >> '"' >>
    star ((range32 0x20 0x7e && not '\\' && not '"') <|> simpleByteEscape <|> hexByteEscape) >>
    '"'

--------------------------------------------------------------------------------
-- Comments and documentation comments.
--
-- Required corner cases:
--   ///!  outer line doc
--   ////  normal line comment
--   //!/  inner line doc
--
-- Maximal munch makes `////` a normal comment because the normal-comment rule
-- consumes the whole line while the outer-doc rule can only consume `///`.
--------------------------------------------------------------------------------
lineCommentTail : RExp False
lineCommentTail = star notLineBreakChar

outerDocLineBody : RExp False
outerDocLineBody =
  opt ((notLineBreakChar && not '/') >> lineCommentTail)

export
outerDocLineComment : RExp True
outerDocLineComment =
  '/' >> '/' >> '/' >> outerDocLineBody

export
innerDocLineComment : RExp True
innerDocLineComment =
  '/' >> '/' >> '!' >> lineCommentTail

export
normalLineComment : RExp True
normalLineComment =
  '/' >> '/' >> lineCommentTail

--------------------------------------------------------------------------------
-- Block comments.
--
-- Rust classifies `/**` as an outer documentation-comment opener only when the
-- following character is neither `*` nor `/`. Thus `/** text */` is a doc
-- comment, while `/**/`, `/***/`, and banner-style `/*** text */` comments are
-- ordinary block comments. Inner block docs may be empty, so `/*!*/` is a
-- valid inner doc comment.
--
-- ilex has no lookahead assertion, so `outerBlockDocOpen` consumes the first
-- body character along with `/**`. The block-comment action preserves that
-- character as part of the raw documentation text.
--------------------------------------------------------------------------------
outerBlockDocFirstBodyChar : RExp True
outerBlockDocFirstBodyChar =
      (notLineBreakChar && not '*' && not '/')
  <|> lineBreak

-- One or more stars beyond the mandatory `*` in `/*`. Under Rust's convention,
-- more than one additional star always begins an ordinary block comment.
additionalBlockCommentStars : RExp True
additionalBlockCommentStars = plus '*'

export
allStarsOuterBlockComment : RExp True
allStarsOuterBlockComment =
  '/' >> '*' >> additionalBlockCommentStars >> '/'

export
outerBlockDocOpen : RExp True
outerBlockDocOpen =
  '/' >> '*' >> '*' >> outerBlockDocFirstBodyChar

-- Covers additional-star runs that should begin an ordinary Rust block comment,
-- including a run cut off at true end of input (for example `/***`). Without
-- this explicit accept, the pinned ilex DFA can lose its shorter `/*` match at
-- the extend-only states in the run and hard-fail instead of reporting an
-- unterminated block comment. It also lets banner-style comments enter the
-- block-comment state after their complete opening star run.
export
bareOuterBlockCommentOpen : RExp True
bareOuterBlockCommentOpen =
  '/' >> '*' >> additionalBlockCommentStars

export
innerBlockDocOpen : RExp True
innerBlockDocOpen =
  '/' >> '*' >> '!'

export
normalBlockCommentOpen : RExp True
normalBlockCommentOpen =
  '/' >> '*'

export
blockCommentClose : RExp True
blockCommentClose =
  '*' >> '/'

export
blockCommentBodyChunk : RExp True
blockCommentBodyChunk =
  plus (notLineBreakChar && not '*' && not '/')

export
blockCommentLineBreak : RExp True
blockCommentLineBreak = lineBreak

export
blockCommentSingleStar : RExp True
blockCommentSingleStar = '*'

export
blockCommentSingleSlash : RExp True
blockCommentSingleSlash = '/'
