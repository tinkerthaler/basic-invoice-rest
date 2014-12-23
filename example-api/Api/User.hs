module Api.User (resource) where

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
import Type.User (User)
import Type.UserInfo (UserInfo (..))
import Type.UserSignupError (UserSignupError (..))
import qualified Type.User     as User
import qualified Type.UserInfo as UserInfo

data UserId = ByName String

-- | User extends the root of the API with a reader containing the ways to identify a user in our URLs.
-- Currently only by the user name.
--type WithUser = ReaderT User.Name BlogApi
type WithUser = ReaderT UserId BlogApi


-- | Defines the /user api end-point.
resource :: Resource BlogApi WithUser UserId () Void
resource = mkResourceReader
  { R.name   = "user" -- Name of the HTTP path segment.
  , R.schema = withListing () $ named [("name", singleBy ByName)]
  , R.list   = const list -- requested by GET /user, gives a paginated listing of users.
  , R.get    = Just get
  --, R.create = Just create -- PUT /user creates a new user
  }

get :: Handler WithUser
get = mkIdHandler xmlJsonO $ \_ i -> do
  muser <- liftIO . atomically . userFromIdentifier i =<< (lift . lift) (asks users)
  case muser of
    Nothing -> throwError NotFound
    Just a  -> return a

userFromIdentifier :: UserId -> TVar (Set User) -> STM (Maybe User)
userFromIdentifier (ByName i) pv = finder <$> readTVar pv
  where
--    finder = F.find ((== i) . User.Name) . Set.toList
    finder = F.find ((== i) . T.unpack . User.name) . Set.toList

list :: ListHandler BlogApi
list = mkListing xmlJsonO $ \r -> do
  usrs <- liftIO . atomically . readTVar =<< asks users
  return . map toUserInfo . take (count r) . drop (offset r) . Set.toList $ usrs

-- | Convert a User into a representation that is safe to show to the public.
toUserInfo :: User -> UserInfo
toUserInfo u = UserInfo { UserInfo.name = User.name u }

create :: Handler BlogApi
create = mkInputHandler (xmlJsonE . xmlJsonO . xmlJsonI) $ \usr -> do
  usrs <- asks users
  merr <- liftIO . atomically $ do
    vu <- validUserName usr <$> readTVar usrs
    if not (validPassword usr)
      then return . Just $ domainReason InvalidPassword
      else if not vu
        then return . Just $ domainReason InvalidUserName
        else modifyTVar usrs (Set.insert usr) >> return Nothing
  maybe (return $ toUserInfo usr) throwError merr

validPassword :: User.User -> Bool
validPassword = (> 1) . T.length . User.password

validUserName :: User -> Set User -> Bool
validUserName u usrs =
  let un        = User.name u
      available = F.all ((un /=). User.name) usrs
      nonEmpty  = (> 1) . T.length $ un
  in available && nonEmpty
