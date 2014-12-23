{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ApiTypes where

import Control.Applicative (Applicative)
import Control.Concurrent.STM (TVar)
import Control.Monad.Reader (MonadReader, ReaderT (..))
import Control.Monad.Trans (MonadIO)
import Data.HashMap.Strict (HashMap)
import Data.Set (Set)

import Type.Comment (Comment)
import Type.Invoice (Invoice)
import Type.Customer (Customer)
import qualified Type.Invoice as Invoice

data ServerData = ServerData
  { customers    :: TVar (Set Customer)
  , invoices    :: TVar (Set Invoice)
  , comments :: TVar (HashMap Invoice.Id (Set Comment))
  }

newtype BlogApi a = BlogApi { unBlogApi :: ReaderT ServerData IO a }
  deriving ( Applicative
           , Functor
           , Monad
           , MonadIO
           , MonadReader ServerData
           )

runBlogApi :: ServerData -> BlogApi a -> IO a
runBlogApi serverdata = flip runReaderT serverdata . unBlogApi
