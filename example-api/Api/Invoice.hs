{-# LANGUAGE DeriveDataTypeable #-}
module Api.Invoice
  ( Identifier (..)
  , WithInvoice
  , resource
  , postFromIdentifier
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
import Type.User (User)
import Type.UserInvoice (UserInvoice (UserInvoice))
import qualified Type.CreateInvoice as CreateInvoice
import qualified Type.Invoice       as Invoice
import qualified Type.User       as User

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
-- Currently only by the title of the post.
type WithInvoice = ReaderT Identifier BlogApi

-- | Defines the /post api end-point.
resource :: Resource BlogApi WithInvoice Identifier () Void
resource = mkResourceReader
  { R.name   = "post" -- Name of the HTTP path segment.
  , R.schema = withListing () $ named [("id", singleRead ById), ("latest", single Latest)]
  , R.list   = const list -- list is requested by GET /post which gives a listing of posts.
  , R.create = Just create -- PUT /post to create a new Invoice.
  , R.get    = Just get
  , R.remove = Just remove
  }

postFromIdentifier :: Identifier -> TVar (Set Invoice) -> STM (Maybe Invoice)
postFromIdentifier i pv = finder <$> readTVar pv
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
  mpost <- liftIO . atomically . postFromIdentifier i =<< (lift . lift) (asks posts)
  case mpost of
    Nothing -> throwError NotFound
    Just a  -> return a

-- | List Invoices with the most recent posts first.
list :: ListHandler BlogApi
list = mkListing xmlJsonO $ \r -> do
  psts <- liftIO . atomically . readTVar =<< asks posts
  return . take (count r) . drop (offset r) . sortBy (flip $ comparing Invoice.createdTime) . Set.toList $ psts

create :: Handler BlogApi
create = mkInputHandler (xmlJsonE . xmlJson) $ \(UserInvoice usr pst) -> do
  -- Make sure the credentials are valid
  checkLogin usr
  pstsVar <- asks posts
  psts <- liftIO . atomically . readTVar $ pstsVar
  post <- liftIO $ toInvoice (Set.size psts + 1) usr pst
  -- Validate and save the post in the same transaction.
  merr <- liftIO . atomically $ do
    let vt = validTitle pst psts
    if not vt
      then return . Just $ domainReason InvalidTitle
      else if not (validContent pst)
        then return . Just $ domainReason InvalidContent
        else modifyTVar pstsVar (Set.insert post) >> return Nothing
  maybe (return post) throwError merr

remove :: Handler WithInvoice
remove = mkIdHandler id $ \_ i -> do
  pstsVar <- lift . lift $ asks posts
  merr <- liftIO . atomically $ do
    mpost <- postFromIdentifier i pstsVar
    case mpost of
      Nothing -> return . Just $ NotFound
      Just post -> modifyTVar pstsVar (Set.delete post) >> return Nothing
  maybe (return ()) throwError merr

-- | Convert a User and CreateInvoice into a Invoice that can be saved.
toInvoice :: Int -> User -> CreateInvoice -> IO Invoice
toInvoice i u p = do
  t <- getCurrentTime
  return Invoice
    { Invoice.id          = i
    , Invoice.author      = User.name u
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

-- | Throw an error if the user isn't logged in.
checkLogin :: User -> ErrorT (Reason e) BlogApi ()
checkLogin usr = do
  usrs <- liftIO . atomically . readTVar =<< asks users
  unless (usr `F.elem` usrs) $ throwError NotAllowed