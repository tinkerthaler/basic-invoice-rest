{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC-fno-warn-unused-imports#-}
module Restexample.Client.Invoice.Comment where
import Rest.Client.Internal
import qualified Restexample.Client.Invoice as Invoice
import qualified Rest.Types.Container
import qualified Type.Comment
import qualified Type.CustomerComment
 
type Identifier = [(Char)]
 
readId :: Identifier -> [String]
readId x = ["id", showUrl x]
 
list ::
       ApiStateC m =>
       Invoice.Identifier ->
         [(String, String)] ->
           m (ApiResponse ()
                (Rest.Types.Container.List (Type.Comment.Comment)))
list invoice pList
  = let rHeaders
          = [(hAccept, "text/json"), (hContentType, "text/plain")]
        request
          = makeReq "GET" "v1.0.0" [["invoice"], Invoice.readId invoice, ["comment"]]
              pList
              rHeaders
              ""
      in doRequest fromJSON fromJSON request
 
create ::
         ApiStateC m =>
         Invoice.Identifier ->
           Type.CustomerComment.CustomerComment ->
             m (ApiResponse () Type.Comment.Comment)
create invoice input
  = let rHeaders
          = [(hAccept, "text/json"), (hContentType, "text/json")]
        request
          = makeReq "POST" "v1.0.0" [["invoice"], Invoice.readId invoice, ["comment"]]
              []
              rHeaders
              (toJSON input)
      in doRequest fromJSON fromJSON request
