{-# LANGUAGE ScopedTypeVariables #-}
module Api.Post.Comment (resource) where

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

import Api.Post (WithPost, postFromIdentifier)
import ApiTypes
import Type.Comment (Comment (Comment))
import Type.UserComment (UserComment (UserComment))
import qualified Type.Comment as Comment
import qualified Type.Post    as Post
import qualified Type.User    as User

type Identifier = String

type WithComment = ReaderT Identifier WithPost

resource :: Resource WithPost WithComment Identifier () Void
resource = mkResourceReader
  { R.name   = "comment"
  , R.schema = withListing () $ named [("id", singleRead id)]
  , R.list   = const list
  , R.create = Just create -- PUT /post to create a new Post.
  , R.remove = Just remove
  }

list :: ListHandler WithPost
list = mkListing xmlJsonO $ \r -> do
  postIdent <- ask
  mpostId <- return . fmap Post.id
         =<< liftIO . atomically . postFromIdentifier postIdent
         =<< (lift . lift) (asks posts)
  case mpostId of
    Nothing -> throwError NotFound
    Just postId -> do
      comms <- liftIO . atomically . readTVar
           =<< (lift . lift) (asks comments)
      return . take (count r) . drop (offset r)
             . sortBy (flip $ comparing Comment.createdTime)
             . maybe [] Set.toList . H.lookup postId $ comms

create :: Handler WithPost
create = mkInputHandler (xmlJson) $ \ucomm -> do
  post  <- undefined -- ask
  comm  <- liftIO $ userCommentToComment ucomm
  comms <- lift . lift $ asks comments
  liftIO . atomically $
    modifyTVar' comms (H.insertWith (<>) post (Set.singleton comm))
  return comm

remove :: Handler WithComment
remove = mkConstHandler xmlJsonO $ return ()

userCommentToComment :: UserComment -> IO Comment
userCommentToComment (UserComment u content) = do
  t <- getCurrentTime
  return $ Comment (User.name u) t content
