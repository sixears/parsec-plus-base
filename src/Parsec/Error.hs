{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax   #-}

module Parsec.Error
  ( AsParseError(..), IOParseError, ParseError( ParseError ) )
where

-- base --------------------------------

import Data.Eq        ( Eq )
import Data.Function  ( id )
import Text.Show      ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- lens --------------------------------

import Control.Lens.Fold    ( (^?) )
import Control.Lens.Getter  ( to )
import Control.Lens.Prism   ( Prism', prism' )
import Control.Lens.TH      ( makePrisms )

-- monadio-error -----------------------

import MonadError.IO.Error  ( AsIOError( _IOError, _IOErr )
                            , IOError( IOErr, unErr ) )

-- parsec --------------------------------

import qualified  Text.Parsec.Error  as  ParsecError

-- text-printer ------------------------

import qualified  Text.Printer  as  P

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

--------------------------------------------------------------------------------

newtype ParseError = ParseError ParsecError.ParseError
  deriving (Eq, Show)

instance Printable ParseError where
  print (ParseError e) = P.string (show e)

------------------------------------------------------------

class AsParseError e where
  _ParseError ∷ Prism' e ParseError

instance AsParseError ParseError where
  _ParseError = id

------------------------------------------------------------

data IOParseError = IOPE_IO_ERROR    IOError
                  | IOPE_PARSE_ERROR ParseError
  deriving (Eq, Show)

$( makePrisms ''IOParseError )

instance AsParseError IOParseError where
  _ParseError = _IOPE_PARSE_ERROR

instance AsIOError IOParseError where
  _IOError = _IOPE_IO_ERROR
  _IOErr   = prism' (IOPE_IO_ERROR ∘ IOErr) (^? _IOPE_IO_ERROR ∘ to unErr)

-- that's all, folks! ----------------------------------------------------------
