{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Control.Exception (Exception(..))
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import qualified Data.ByteString.Char8      as C8
import Data.ByteString.Short (ShortByteString)
import Data.Functor.Identity

import Control.Monad.Fix
import Data.ByteString (ByteString)
import Data.List (intercalate)
import Data.List.NonEmpty (head)
import Data.Word (Word8)
import Prelude hiding (head)
import Text.Megaparsec
import qualified Text.Megaparsec.Error as E
import Text.Megaparsec.Pos (SourcePos)


newtype QrulT e m a = QrulT {unC4 :: ExceptT e m a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadTrans, MonadError e)

runQrulT :: QrulT e m a -> m (Either e a)
runQrulT = runExceptT . unC4

type Qrul e = QrulT e Identity
runQrul :: Qrul e a -> Either e a
runQrul = runIdentity . runQrulT

instance Exception CodegenError where
  displayException (CodegenError txt pos) = sourcePosPretty pos <> ": error: " <> txt <> "\n"

data CodegenError = CodegenError String SourcePos deriving (Show)
