{-# LANGUAGE DeriveDataTypeable #-}
module Api.Invoice
  ( Identifier (..)
  , WithInvoice
  , resource
  , invoiceFromIdentifier
  ) where

import Control.Applicative
import Control.Concurrent.STM (STM, TVar, atomically, modifyTVar, readTVar)
import Control.Monad (unless)
import Control.Monad.Error (ErrorT, throwError)
import Control.Monad.Reader (ReaderT, asks)
import Control.Monad.Trans (lift, liftIO)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Time
import Data.Typeable
import Safe
import qualified Data.Foldable as F
import qualified Data.Set      as Set
import qualified Data.Text     as T

import Rest
import Rest.Info
import Rest.ShowUrl
import qualified Rest.Resource as R

import ApiTypes
import Type.CreateInvoice (CreateInvoice)
import Type.Invoice (Invoice (Invoice))
import Type.InvoiceError (InvoiceError (..))
import Type.Customer (Customer)
import Type.CustomerInvoice (CustomerInvoice (CustomerInvoice))
import qualified Type.CreateInvoice as CreateInvoice
import qualified Type.Invoice       as Invoice
import qualified Type.Customer       as Customer

data Identifier
  = Latest
  | ById Int
  deriving (Eq, Show, Read, Typeable)

instance Info Identifier where
  describe _ = "identifier"

instance ShowUrl Identifier where
  showUrl Latest = "latest"
  showUrl (ById i) = show i

-- | Invoice extends the root of the API with a reader containing the ways to identify a Invoice in our URLs.
-- Currently only by the title of the invoice.
type WithInvoice = ReaderT Identifier BlogApi

-- | Defines the /invoice api end-point.
resource :: Resource BlogApi WithInvoice Identifier () Void
resource = mkResourceReader
  { R.name   = "invoice" -- Name of the HTTP path segment.
  , R.schema = withListing () $ named [("id", singleRead ById), ("latest", single Latest)]
  , R.list   = const list -- list is requested by GET /invoice which gives a listing of invoices.
  , R.create = Just create -- PUT /invoice to create a new Invoice.
  , R.get    = Just get
  , R.remove = Just remove
  }

invoiceFromIdentifier :: Identifier -> TVar (Set Invoice) -> STM (Maybe Invoice)
invoiceFromIdentifier i pv = finder <$> readTVar pv
  where
    finder = case i of
      ById ident -> F.find ((== ident) . Invoice.id) . Set.toList
      Latest     -> headMay . sortBy (flip $ comparing Invoice.createdTime) . Set.toList

{--
 -- See Tutorial
get :: Handler (ReaderT Title IO)
get :: mkIdHandler xmlJsonO $ \_ title -> liftIO readInvoiceFromDb title
 --}

get :: Handler WithInvoice
get = mkIdHandler xmlJsonO $ \_ i -> do
  minvoice <- liftIO . atomically . invoiceFromIdentifier i =<< (lift . lift) (asks invoices)
  case minvoice of
    Nothing -> throwError NotFound
    Just a  -> return a

-- | List Invoices with the most recent invoices first.
list :: ListHandler BlogApi
list = mkListing xmlJsonO $ \r -> do
  psts <- liftIO . atomically . readTVar =<< asks invoices
  return . take (count r) . drop (offset r) . sortBy (flip $ comparing Invoice.createdTime) . Set.toList $ psts

create :: Handler BlogApi
create = mkInputHandler (xmlJsonE . xmlJson) $ \(CustomerInvoice usr pst) -> do
  -- Make sure the credentials are valid
  checkLogin usr
  pstsVar <- asks invoices
  psts <- liftIO . atomically . readTVar $ pstsVar
  invoice <- liftIO $ toInvoice (Set.size psts + 1) usr pst
  -- Validate and save the invoice in the same transaction.
  merr <- liftIO . atomically $ do
    let vt = validTitle pst psts
    if not vt
      then return . Just $ domainReason InvalidTitle
      else if not (validContent pst)
        then return . Just $ domainReason InvalidContent
        else modifyTVar pstsVar (Set.insert invoice) >> return Nothing
  maybe (return invoice) throwError merr

remove :: Handler WithInvoice
remove = mkIdHandler id $ \_ i -> do
  pstsVar <- lift . lift $ asks invoices
  merr <- liftIO . atomically $ do
    minvoice <- invoiceFromIdentifier i pstsVar
    case minvoice of
      Nothing -> return . Just $ NotFound
      Just invoice -> modifyTVar pstsVar (Set.delete invoice) >> return Nothing
  maybe (return ()) throwError merr

-- | Convert a Customer and CreateInvoice into a Invoice that can be saved.
toInvoice :: Int -> Customer -> CreateInvoice -> IO Invoice
toInvoice i u p = do
  t <- getCurrentTime
  return Invoice
    { Invoice.id          = i
    , Invoice.author      = Customer.name u
    , Invoice.createdTime = t
    , Invoice.title       = CreateInvoice.title p
    , Invoice.content     = CreateInvoice.content p
    }

-- | A Invoice's title must be unique and non-empty.
validTitle :: CreateInvoice -> Set Invoice -> Bool
validTitle p psts =
  let pt        = CreateInvoice.title p
      nonEmpty  = (>= 1) . T.length $ pt
      available = F.all ((pt /=) . Invoice.title) psts
  in available && nonEmpty

-- | A Invoice's content must be non-empty.
validContent :: CreateInvoice -> Bool
validContent = (>= 1) . T.length . CreateInvoice.content

-- | Throw an error if the customer isn't logged in.
checkLogin :: Customer -> ErrorT (Reason e) BlogApi ()
checkLogin usr = do
  usrs <- liftIO . atomically . readTVar =<< asks customers
  unless (usr `F.elem` usrs) $ throwError NotAllowed
