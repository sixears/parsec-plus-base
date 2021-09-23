module Parsec.Error
  ( AsParseError(..), IOParseError, ParseError( ParseError ) )
where

-- base --------------------------------

import Data.Eq        ( Eq( (==) ) )
import Data.Function  ( (&), id )
import GHC.Generics   ( Generic )
import GHC.Stack      ( CallStack )
import Text.Show      ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData( rnf ), deepseq )

-- has-callstack -----------------------

import HasCallstack  ( HasCallstack( callstack ) )

-- lens --------------------------------

import Control.Lens.Fold    ( (^?) )
import Control.Lens.Lens    ( lens )
import Control.Lens.Prism   ( Prism', prism, prism' )

-- monadio-error -----------------------

import MonadError.IO.Error  ( AsIOError( _IOError, _IOErr )
                            , IOError, ioErr )

-- more-unicode ------------------------

import Data.MoreUnicode.Either   ( pattern 𝕷, pattern 𝕽 )
import Data.MoreUnicode.Lens     ( (⊣), (⊢) )
import Data.MoreUnicode.Functor  ( (⊳) )

-- parsec --------------------------------

import qualified  Text.Parsec.Error  as  ParsecError

import Text.Parsec.Pos  ( sourceColumn, sourceLine, sourceName )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

--------------------------------------------------------------------------------

data ParseError = ParseError { _pe        ∷ ParsecError.ParseError
                             , _callstack ∷ CallStack }
  deriving (Generic, Show)

----------

instance Eq ParseError where
  (ParseError a _) == (ParseError b _) = a == b

----------

instance HasCallstack ParseError where
  callstack = lens _callstack (\ pe cs → pe { _callstack = cs })

----------

instance NFData ParseError where
  rnf (ParseError e cs) =
    let pos  = ParsecError.errorPos e
        line = sourceLine pos
        col  = sourceColumn pos
        name = sourceName pos
        deep_pos = line `deepseq` col `deepseq` name `deepseq` ()
        deep_mess (ParsecError.SysUnExpect s) = s `deepseq` ()
        deep_mess (ParsecError.UnExpect    s) = s `deepseq` ()
        deep_mess (ParsecError.Expect      s) = s `deepseq` ()
        deep_mess (ParsecError.Message     s) = s `deepseq` ()
        deep_messes = deep_mess ⊳ ParsecError.errorMessages e
     in cs `deepseq` deep_pos `deepseq` deep_messes `deepseq` ()

----------

instance Printable ParseError where
  print (ParseError e _) = P.string (show e)

------------------------------------------------------------

class AsParseError ε where
  _ParseError ∷ Prism' ε ParseError

instance AsParseError ParseError where
  _ParseError = id

------------------------------------------------------------

data IOParseError = IOPE_IO_ERROR    IOError
                  | IOPE_PARSE_ERROR ParseError
  deriving (Generic, Eq, NFData, Show)

_IOPE_IO_ERROR ∷ Prism' IOParseError IOError
_IOPE_IO_ERROR =
  prism (\ ioe → IOPE_IO_ERROR ioe) (\ iope → case iope of
                                        IOPE_IO_ERROR ioe → 𝕽 ioe
                                        _                 → 𝕷 iope)

_IOPE_PARSE_ERROR ∷ Prism' IOParseError ParseError
_IOPE_PARSE_ERROR =
  (prism (\ pe → IOPE_PARSE_ERROR pe)) (\ iope → case iope of
                                           IOPE_PARSE_ERROR pe → 𝕽 pe
                                           _                   → 𝕷 iope)

--------------------

instance HasCallstack IOParseError where
  callstack =
    let getter (IOPE_IO_ERROR    e) = e ⊣ callstack
        getter (IOPE_PARSE_ERROR e) = e ⊣ callstack
        setter (IOPE_IO_ERROR    e) cs = IOPE_IO_ERROR    (e & callstack ⊢ cs)
        setter (IOPE_PARSE_ERROR e) cs = IOPE_PARSE_ERROR (e & callstack ⊢ cs)
     in lens getter setter

instance Printable IOParseError where
  print (IOPE_PARSE_ERROR e) = print e
  print (IOPE_IO_ERROR    e) = print e

instance AsParseError IOParseError where
  _ParseError = _IOPE_PARSE_ERROR

instance AsIOError IOParseError where
  _IOError = _IOPE_IO_ERROR
  _IOErr   = prism' (IOPE_IO_ERROR ∘ ioErr) (^? _IOPE_IO_ERROR ∘ _IOErr)

-- that's all, folks! ----------------------------------------------------------
