{-# LANGUAGE ScopedTypeVariables #-}
module Api.Invoice.Comment (resource) where

import Control.Concurrent.STM (atomically, modifyTVar', readTVar)
import Control.Monad.Reader
import Control.Monad.Trans.Error
import Data.List
import Data.Monoid
import Data.Ord
import Data.Time
import qualified Data.HashMap.Strict as H
import qualified Data.Set            as Set

import Rest
import qualified Rest.Resource as R

import Api.Invoice (WithInvoice, invoiceFromIdentifier)
import ApiTypes
import Type.Comment (Comment (Comment))
import Type.CustomerComment (CustomerComment (CustomerComment))
import qualified Type.Comment as Comment
import qualified Type.Invoice    as Invoice
import qualified Type.Customer    as Customer

type Identifier = String

type WithComment = ReaderT Identifier WithInvoice

resource :: Resource WithInvoice WithComment Identifier () Void
resource = mkResourceReader
  { R.name   = "comment"
  , R.schema = withListing () $ named [("id", singleRead id)]
  , R.list   = const list
  , R.create = Just create -- PUT /invoice to create a new Invoice.
  }

list :: ListHandler WithInvoice
list = mkListing xmlJsonO $ \r -> do
  invoiceId <- getInvoiceId `orThrow` NotFound
  comms <- liftIO . atomically . readTVar
       =<< (lift . lift) (asks comments)
  return . take (count r) . drop (offset r)
         . sortBy (flip $ comparing Comment.createdTime)
         . maybe [] Set.toList . H.lookup invoiceId $ comms

create :: Handler WithInvoice
create = mkInputHandler xmlJson $ \ucomm -> do
  invoiceId <- getInvoiceId `orThrow` NotFound
  comm   <- liftIO $ customerCommentToComment ucomm
  comms  <- lift . lift $ asks comments
  liftIO . atomically $
    modifyTVar' comms (H.insertWith (<>) invoiceId (Set.singleton comm))
  return comm

getInvoiceId :: ErrorT (Reason ()) WithInvoice (Maybe Invoice.Id)
getInvoiceId = do
  invoiceIdent <- ask
  return . fmap Invoice.id
        =<< liftIO . atomically . invoiceFromIdentifier invoiceIdent
        =<< (lift . lift) (asks invoices)

customerCommentToComment :: CustomerComment -> IO Comment
customerCommentToComment (CustomerComment u content) = do
  t <- getCurrentTime
  return $ Comment (Customer.name u) t content
