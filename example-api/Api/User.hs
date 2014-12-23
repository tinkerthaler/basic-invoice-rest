module Api.Customer (resource) where

import Control.Applicative ((<$>))
import Control.Concurrent.STM (STM, TVar, atomically, modifyTVar, readTVar)
import Control.Monad.Error (throwError)
import Control.Monad.Reader (ReaderT, asks)
import Control.Monad.Trans (lift, liftIO)
import Data.Set (Set)
import qualified Data.Foldable as F
import qualified Data.Set      as Set
import qualified Data.Text     as T

import Rest (Handler, ListHandler, Range (count, offset), Resource, Void, domainReason, mkInputHandler, mkListing, mkResourceReader, named, singleRead,
             withListing, xmlJsonE, xmlJsonI, xmlJsonO)
import qualified Rest.Resource as R
import Rest.Error (Reason (NotFound))
import Rest.Handler (mkIdHandler)
import Rest.Schema (singleBy, noListing)

import ApiTypes (BlogApi, ServerData (..))
import Type.Customer (Customer)
import Type.CustomerInfo (CustomerInfo (..))
import Type.CustomerSignupError (CustomerSignupError (..))
import qualified Type.Customer     as Customer
import qualified Type.CustomerInfo as CustomerInfo

data CustomerId = ByName String

-- | Customer extends the root of the API with a reader containing the ways to identify a customer in our URLs.
-- Currently only by the customer name.
--type WithCustomer = ReaderT Customer.Name BlogApi
type WithCustomer = ReaderT CustomerId BlogApi


-- | Defines the /customer api end-point.
resource :: Resource BlogApi WithCustomer CustomerId () Void
resource = mkResourceReader
  { R.name   = "customer" -- Name of the HTTP path segment.
  , R.schema = withListing () $ named [("name", singleBy ByName)]
  , R.list   = const list -- requested by GET /customer, gives a paginated listing of customers.
  , R.get    = Just get
  --, R.create = Just create -- PUT /customer creates a new customer
  }

get :: Handler WithCustomer
get = mkIdHandler xmlJsonO $ \_ i -> do
  mcustomer <- liftIO . atomically . customerFromIdentifier i =<< (lift . lift) (asks customers)
  case mcustomer of
    Nothing -> throwError NotFound
    Just a  -> return a

customerFromIdentifier :: CustomerId -> TVar (Set Customer) -> STM (Maybe Customer)
customerFromIdentifier (ByName i) pv = finder <$> readTVar pv
  where
--    finder = F.find ((== i) . Customer.Name) . Set.toList
    finder = F.find ((== i) . T.unpack . Customer.name) . Set.toList

list :: ListHandler BlogApi
list = mkListing xmlJsonO $ \r -> do
  usrs <- liftIO . atomically . readTVar =<< asks customers
  return . map toCustomerInfo . take (count r) . drop (offset r) . Set.toList $ usrs

-- | Convert a Customer into a representation that is safe to show to the public.
toCustomerInfo :: Customer -> CustomerInfo
toCustomerInfo u = CustomerInfo { CustomerInfo.name = Customer.name u }

create :: Handler BlogApi
create = mkInputHandler (xmlJsonE . xmlJsonO . xmlJsonI) $ \usr -> do
  usrs <- asks customers
  merr <- liftIO . atomically $ do
    vu <- validCustomerName usr <$> readTVar usrs
    if not (validPassword usr)
      then return . Just $ domainReason InvalidPassword
      else if not vu
        then return . Just $ domainReason InvalidCustomerName
        else modifyTVar usrs (Set.insert usr) >> return Nothing
  maybe (return $ toCustomerInfo usr) throwError merr

validPassword :: Customer.Customer -> Bool
validPassword = (> 1) . T.length . Customer.password

validCustomerName :: Customer -> Set Customer -> Bool
validCustomerName u usrs =
  let un        = Customer.name u
      available = F.all ((un /=). Customer.name) usrs
      nonEmpty  = (> 1) . T.length $ un
  in available && nonEmpty
