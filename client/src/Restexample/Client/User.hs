{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC-fno-warn-unused-imports#-}
module Restexample.Client.User where
import Rest.Client.Internal
import qualified Rest.Types.Container
import qualified Type.UserInfo
import qualified Type.User
 
type Identifier = String
 
readId :: Identifier -> [String]
readId x = ["name", showUrl x]
 
list ::
       ApiStateC m =>
       [(String, String)] ->
         m (ApiResponse ()
              (Rest.Types.Container.List (Type.UserInfo.UserInfo)))
list pList
  = let rHeaders
          = [(hAccept, "text/json"), (hContentType, "text/plain")]
        request = makeReq "GET" "v1.0.0" [["user"]] pList rHeaders ""
      in doRequest fromJSON fromJSON request
 
byName ::
         ApiStateC m => String -> m (ApiResponse () Type.User.User)
byName string
  = let rHeaders
          = [(hAccept, "text/json"), (hContentType, "text/plain")]
        request
          = makeReq "GET" "v1.0.0" [["user"], ["name"], [showUrl string]] []
              rHeaders
              ""
      in doRequest fromJSON fromJSON request