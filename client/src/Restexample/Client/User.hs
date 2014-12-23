{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC-fno-warn-unused-imports#-}
module Restexample.Client.Customer where
import Rest.Client.Internal
import qualified Rest.Types.Container
import qualified Type.CustomerInfo
import qualified Type.Customer
 
type Identifier = String
 
readId :: Identifier -> [String]
readId x = ["name", showUrl x]
 
list ::
       ApiStateC m =>
       [(String, String)] ->
         m (ApiResponse ()
              (Rest.Types.Container.List (Type.CustomerInfo.CustomerInfo)))
list pList
  = let rHeaders
          = [(hAccept, "text/json"), (hContentType, "text/plain")]
        request = makeReq "GET" "v1.0.0" [["customer"]] pList rHeaders ""
      in doRequest fromJSON fromJSON request
 
byName ::
         ApiStateC m => String -> m (ApiResponse () Type.Customer.Customer)
byName string
  = let rHeaders
          = [(hAccept, "text/json"), (hContentType, "text/plain")]
        request
          = makeReq "GET" "v1.0.0" [["customer"], ["name"], [showUrl string]] []
              rHeaders
              ""
      in doRequest fromJSON fromJSON request
