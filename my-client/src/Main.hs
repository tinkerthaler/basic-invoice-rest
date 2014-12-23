module Main (main) where

import           Rest.Client.Base
import qualified Restexample.Client.Post as Post

--main :: ApiT IO ()
main = do
    res <- runWithPort "localhost" 3000 (Post.list [])
    print res

