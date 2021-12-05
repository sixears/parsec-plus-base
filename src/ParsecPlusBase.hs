module ParsecPlusBase
  ( AsParseError(..), IOParseError, Parsecable(..), Parser
  , boundedDoubledChars, caseInsensitiveChar, caseInsensitiveString, digits
  , parens, __parsecN__, parse, uniquePrefix
  )
where

import Prelude  ( error )

-- base --------------------------------

import Control.Applicative    ( many, pure )
import Control.Monad          ( return, sequence )
import Control.Monad.Fail     ( fail )
import Data.Char              ( Char, toLower, toUpper )
import Data.Either            ( either )
import Data.Eq                ( Eq )
import Data.Function          ( ($), id )
import Data.Functor           ( fmap )
import Data.Functor.Identity  ( Identity )
import Data.List              ( filter, isPrefixOf )
import Data.Monoid            ( mappend )
import Data.String            ( String )
import Data.Tuple             ( fst )
import Data.Word              ( Word8 )
import GHC.Stack              ( HasCallStack )
import Text.Read              ( read )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable, toString )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊵), (⋪), (⋫), (∤) )
import Data.MoreUnicode.Either       ( 𝔼 )
import Data.MoreUnicode.Functor      ( (⊳) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- parsec ------------------------------

import qualified Text.Parsec.Prim

import Text.Parsec.Char        ( char, digit, noneOf, oneOf, string )
import Text.Parsec.Combinator  ( between, choice, count, eof, many1 )
import Text.Parsec.Pos         ( SourceName )
import Text.Parsec.Prim        ( Parsec, ParsecT, Stream, try )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Parsec.Error  ( AsParseError( _ParseError ), IOParseError
                     , ParseError, throwAsParseError )

-------------------------------------------------------------------------------

type Parser α = ∀ s u m . Stream s m Char ⇒ ParsecT s u m α

__right__ ∷ Printable ε ⇒ 𝔼 ε β → β
__right__ x = either (error ∘ toString) id x

class Parsecable χ where
  {- | Parser for a type.  Implement this to benefit from `parsec` and variants.
   -}
  parser ∷ Stream s Identity Char ⇒ Parsec s u χ

  ------------------

  {- | A bit like `Text.Read.read` for parsecable values -}
  parsec ∷ ∀ ε μ s σ .
           (AsParseError ε, MonadError ε μ, Stream s Identity Char, Printable σ,
            HasCallStack)
         ⇒ σ → s → μ χ
  parsec sourceName t = parse parser (toString sourceName) t
{-
    case parse parser (toString sourceName) t of
      Left  e → throwError (_ParseError # ParseError e callStack)
      Right s → return s
-}

  ------------------

  {- | *PARTIAL*: `parsec`, will error on failure to parse -}
  __parsec__ ∷ ∀ s σ . (Printable σ, Stream s Identity Char) ⇒ σ → s → χ
  __parsec__ sourceName = __right__ ∘ parsec @_ @ParseError sourceName

  parsec' ∷ (MonadError ParseError μ, Stream s Identity Char, Printable σ) ⇒
            σ → s → μ χ
  parsec' = parsec
{-# DEPRECATED parsec' "use parsec @ParseError instead" #-}

----------------------------------------

instance Parsecable Word8 where
  {-| parse a word8 value in denary; that is, 0-255 -}
  parser =
    read ⊳ go
    where go = try (mappend ⊳ string "25" ⊵ (pure ⊳ oneOf "012345"))
             ∤ try ((:) ⊳ char '2' ⊵ ((:) ⊳ oneOf "01234" ⊵ count 1 digit))
             ∤ try ((:) ⊳ oneOf "01" ⊵ count 2 digit)
             ∤ try (count 2 digit)
             ∤ count 1 digit

------------------------------------------------------------

{- | *PARTIAL*: Parse a stream, throwing an error in case of failure.  No
     name for the stream is given -}

__parsecN__ ∷ (Stream s Identity Char, Parsecable χ, HasCallStack) ⇒ s → χ
__parsecN__ t = __right__ $ parse @ParseError (parser ⋪ eof) "" t

----------------------------------------

{- | Parse between parentheses -}
{-# DEPRECATED parens "use ParserPlus.parens" #-}
parens ∷ Stream s m Char ⇒ ParsecT s u m a → ParsecT s u m a
parens = between (char '(') (char ')')

----------------------------------------

{- | Parse 1 or more digits -}
{-# DEPRECATED digits "use ParserPlus.digits" #-}
digits ∷ Stream s m Char ⇒ ParsecT s u m String
digits = many1 digit

{-
eChar ∷ Char
eChar = '\\'

escape :: Parser String
escape = pure ⊳ oneOf "\\\"0nrvtbf{}"

nonEscape :: Parser Char
nonEscape = noneOf "\\\"\0\n\r\v\t\b\f{}"

character :: Parser String
character = fmap return nonEscape <|> escape

parseEscaped ∷ String → String → Parser String
parseEscaped l r = do
    strings <- string l *> many character <* string r
    return $ concat strings
-}

----------------------------------------

{- | Parse any character except those in `cs`; they must be doubled.  Thus

     @ parse (many (try $ doubledChar "{}")) "test" "o}}{{p}" ≡ Right "o}{p" @

     Note the use of `try`; doubleChar will consume the first char of
     non-conformant input.
 -}
{-# DEPRECATED doubledChar "use ParserPlus.doubledChar" #-}
doubledChar ∷ [Char] → Parser Char
doubledChar cs = (choice $ (\ c → char c ⋫ char c) ⊳ cs) ∤ noneOf cs

----------------------------------------

{- | Parse many characters, most directly, but those in `cs` must be doubled up.

     @ parse (doubledChars "{}") "test" "o}}{{p}x" ≡ Right "o}{p" @
 -}
{-# DEPRECATED doubledChars "use ParserPlus.doubledChars" #-}
doubledChars ∷ [Char] → Parser [Char]
doubledChars cs = many (try $ doubledChar cs)

----------------------------------------

{- | Parse many characters, most directly, bounded by `l` on the left and `r`
     on the right; instances of `l` & `r` within the text must be doubled up.

     @ parse (boundedDoubledChars '{' '}') "test" "{o}}{{p}x" ≡ Right "o}{p" @

     @ parse (boundedDoubledChars '!' '!') "test" "!o}}!!p!" ≡ Right "o}}!p" @
 -}
{-# DEPRECATED boundedDoubledChars "use ParserPlus.boundedDoubledChars" #-}
boundedDoubledChars ∷ Char -> Char → Parser [Char]
boundedDoubledChars l r = char l ⋫ doubledChars [l,r] ⋪ char r

----------------------------------------

{- | Parse a uniquely matching prefix.

     Given a value table, and a parser; can we parse to something that uniquely
     provides a result?  The parser succeeds if the parse output prefixes
     precisely one result.
 -}
{-# DEPRECATED uniquePrefix "use ParserPlus.uniquePrefix" #-}
uniquePrefix ∷ ∀ α β χ σ τ η . (Eq α, Printable χ) ⇒
               [([α],β)] → ([α] → χ) → ParsecT σ τ η [α] → ParsecT σ τ η β
uniquePrefix ss e prs = do
  s ← prs
  case filter ((s `isPrefixOf`) ∘ fst) ss of
    [(_,y)] → return y
    _       → fail $ toString (e s)

----------------------------------------

{- | Parse the given character, or the same character in another case
     (upper or lower). -}
{-# DEPRECATED caseInsensitiveChar "use ParserPlus.caseInsensitiveChar" #-}
caseInsensitiveChar ∷ Stream σ η Char ⇒ Char → ParsecT σ υ η Char
caseInsensitiveChar c = do
  _ ← char (toLower c) ∤ char (toUpper c)
  return c

-- | Parse the given string, but with any combination of upper and lower case
-- characters.
{-# DEPRECATED caseInsensitiveString "use ParserPlus.caseInsensitiveString" #-}
caseInsensitiveString ∷ Stream σ η Char ⇒ String → ParsecT σ υ η String
caseInsensitiveString = sequence ∘ fmap caseInsensitiveChar

----------------------------------------

parse ∷ ∀ ε α σ τ η . (AsParseError ε, MonadError ε η, Stream σ Identity τ) ⇒
        Parsec σ () α → SourceName → σ → η α
parse p s t = either throwAsParseError return $ Text.Parsec.Prim.parse p s t

-- that's all, folks! ---------------------------------------------------------
