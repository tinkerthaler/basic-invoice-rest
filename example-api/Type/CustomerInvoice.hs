{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  , TemplateHaskell
  , TypeFamilies
  #-}
module Type.CustomerInvoice where

import Data.Aeson
import Data.JSON.Schema
import Data.Typeable
import GHC.Generics
import Generics.Regular
import Generics.Regular.XmlPickler
import Text.XML.HXT.Arrow.Pickle

import Type.CreateInvoice (CreateInvoice)
import Type.Customer (Customer)

data CustomerInvoice = CustomerInvoice { customer :: Customer, invoice :: CreateInvoice }
  deriving (Eq, Generic, Ord, Show, Typeable)

deriveAll ''CustomerInvoice "PFCustomerInvoice"
type instance PF CustomerInvoice = PFCustomerInvoice

instance XmlPickler CustomerInvoice where xpickle = gxpickle
instance JSONSchema CustomerInvoice where schema = gSchema
instance FromJSON   CustomerInvoice
instance ToJSON     CustomerInvoice
