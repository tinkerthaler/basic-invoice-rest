{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC-fno-warn-unused-imports#-}
module Restexample.Client.Invoice where
import Rest.Client.Internal
import qualified Rest.Types.Container
import qualified Type.Invoice
import qualified Rest.StringMap.HashMap.Strict
import qualified Rest.Types.Error
import qualified Type.InvoiceError
import qualified Type.CustomerInvoice
 
data Identifier = Id Int
                | Latest
 
readId :: Identifier -> [String]
readId (Id x) = ["id", showUrl x]
readId Latest = ["latest"]
 
list ::
       ApiStateC m =>
       [(String, String)] ->
         m (ApiResponse () (Rest.Types.Container.List (Type.Invoice.Invoice)))
list pList
  = let rHeaders
          = [(hAccept, "text/json"), (hContentType, "text/plain")]
        request = makeReq "GET" "v1.0.0" [["invoice"]] pList rHeaders ""
      in doRequest fromJSON fromJSON request
 
byId :: ApiStateC m => Int -> m (ApiResponse () Type.Invoice.Invoice)
byId integer
  = let rHeaders
          = [(hAccept, "text/json"), (hContentType, "text/plain")]
        request
          = makeReq "GET" "v1.0.0" [["invoice"], ["id"], [showUrl integer]] []
              rHeaders
              ""
      in doRequest fromJSON fromJSON request
 
removeManyId ::
               ApiStateC m =>
               Rest.StringMap.HashMap.Strict.StringHashMap ([(Char)]) (()) ->
                 m (ApiResponse ()
                      (Rest.StringMap.HashMap.Strict.StringHashMap ([(Char)])
                         (Rest.Types.Error.Status (Rest.Types.Error.Reason (())) (()))))
removeManyId input
  = let rHeaders
          = [(hAccept, "text/json"), (hContentType, "text/json")]
        request
          = makeReq "DELETE" "v1.0.0" [["invoice"], ["id"]] [] rHeaders
              (toJSON input)
      in doRequest fromJSON fromJSON request
 
latest :: ApiStateC m => m (ApiResponse () Type.Invoice.Invoice)
latest
  = let rHeaders
          = [(hAccept, "text/json"), (hContentType, "text/plain")]
        request
          = makeReq "GET" "v1.0.0" [["invoice"], ["latest"]] [] rHeaders ""
      in doRequest fromJSON fromJSON request
 
create ::
         ApiStateC m =>
         Type.CustomerInvoice.CustomerInvoice ->
           m (ApiResponse Type.InvoiceError.InvoiceError Type.Invoice.Invoice)
create input
  = let rHeaders
          = [(hAccept, "text/json"), (hContentType, "text/json")]
        request
          = makeReq "POST" "v1.0.0" [["invoice"]] [] rHeaders (toJSON input)
      in doRequest fromJSON fromJSON request
 
remove :: ApiStateC m => Identifier -> m (ApiResponse () ())
remove invoice
  = let rHeaders
          = [(hAccept, "text/json"), (hContentType, "text/plain")]
        request
          = makeReq "DELETE" "v1.0.0" [["invoice"], readId invoice] [] rHeaders ""
      in doRequest fromJSON (const ()) request
