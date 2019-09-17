{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnicodeSyntax    #-}

{- | A Parsecable class, plus some extra helpful utilities.  Base version, so
     fpath can use it, and ParsecPlus proper can use that (for file parsing). -}
module ParsecPlusBase
  ( AsParseError(..), IOParseError, Parsecable(..)
  , digits, parens, __parsecN__ )
where

import Prelude  ( error )

-- base --------------------------------

import Control.Applicative    ( pure )
import Control.Monad          ( return )
import Data.Bifunctor         ( first )
import Data.Char              ( Char )
import Data.Either            ( Either( Left, Right ), either )
import Data.Function          ( ($), id )
import Data.Functor.Identity  ( Identity )
import Data.Monoid            ( mappend )
import Data.String            ( String )
import Data.Word              ( Word8 )
import Text.Read              ( read )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable, toString )

-- lens --------------------------------

import Control.Lens.Review   ( (#) )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative   ( (⊵), (∤), (⋪) )
import Data.MoreUnicode.Functor       ( (⊳) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError, throwError )

-- parsec ------------------------------

import Text.Parsec.Char        ( char, digit, oneOf, string )
import Text.Parsec.Combinator  ( between, count, eof, many1 )
import Text.Parsec.Prim        ( Parsec, ParsecT, Stream, parse, try )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Parsec.Error  ( AsParseError( _ParseError ), IOParseError
                     , ParseError( ParseError ) )

--------------------------------------------------------------------------------

__right__ ∷ Printable ε ⇒ Either ε β → β
__right__ x = either (error ∘ toString) id x

class Parsecable χ where
  {- | Parser for a type.  Implement this to benefit from `parsec` and variants.
   -}
  parser ∷ Stream s Identity Char ⇒ Parsec s u χ

  ------------------

  {- | A bit like `Text.Read.read` for parsecable values -}
  parsec ∷ (AsParseError ε, MonadError ε μ, Stream s Identity Char, Printable σ)
         ⇒ σ → s → μ χ
  parsec sourceName t = case parse parser (toString sourceName) t of
                           Left  e → throwError (_ParseError # ParseError e)
                           Right s → return s

  ------------------

  {- | Like `parsec`, with error type reified to `ParseError`. -}
  parsec' ∷ (MonadError ParseError μ, Stream s Identity Char, Printable σ) ⇒
             σ → s → μ χ
  parsec' = parsec

  ------------------

  {- | *PARTIAL*: `parsec`, will error on failure to parse -}
  __parsec__ ∷ (Printable σ, Stream s Identity Char) ⇒ σ → s → χ
  __parsec__ sourceName = __right__ ∘ parsec' sourceName

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

__parsecN__ ∷ (Stream s Identity Char, Parsecable χ) ⇒ s → χ
__parsecN__ t = __right__ ∘ first ParseError $ parse (parser ⋪ eof) "" t

----------------------------------------

{- | Parse between parentheses -}
parens ∷ Stream s m Char ⇒ ParsecT s u m a → ParsecT s u m a
parens = between (char '(') (char ')')

----------------------------------------

{- | Parse 1 or more digits -}
digits ∷ Stream s m Char ⇒ ParsecT s u m String
digits = many1 digit

-- that's all, folks! ----------------------------------------------------------
