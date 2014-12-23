{-# LANGUAGE OverloadedStrings #-}
module Example (exampleBlog) where

import Control.Applicative
import Control.Concurrent.STM (newTVarIO)
import Data.HashMap.Strict (HashMap)
import Data.Set (Set)
import qualified Data.HashMap.Strict as H
import qualified Data.Set            as Set

import ApiTypes (ServerData (..))
import Type.Comment (Comment (Comment))
import Type.Invoice (Invoice (Invoice))
import Type.Customer (Customer (Customer))

-- Set up the server state
exampleBlog :: IO ServerData
exampleBlog = ServerData
          <$> newTVarIO mockCustomers
          <*> newTVarIO mockInvoices
          <*> newTVarIO mockComments

-- | Prepoulated customers
mockCustomers :: Set Customer
mockCustomers = Set.fromList
  [ Customer "adam" "1234"
  , Customer "erik" "2345"
  , Customer "sebas" "3456"
  ]

-- | Prepopulated invoices
mockInvoices :: Set Invoice
mockInvoices = Set.fromList
  [ Invoice 0 "adam" (read "2014-03-31 15:34:00") "First invoice" "Hello world!"
  , Invoice 1 "erik" (read "2014-04-01 13:37:00") "Rest is awesome" "Just wanted to tell the world!"
  ]

mockComments :: HashMap Int (Set Comment)
mockComments = H.fromList
  [(0, Set.fromList [Comment "adam" (read "2014-06-08 14:00:00") "This is the best invoice I've ever written, please be gentle"])]
