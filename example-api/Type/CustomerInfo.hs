{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  , TemplateHaskell
  , TypeFamilies
  #-}
module Type.CustomerInfo where

import Data.Aeson
import Data.JSON.Schema
import Data.Typeable
import GHC.Generics
import Generics.Regular
import Generics.Regular.XmlPickler
import Text.XML.HXT.Arrow.Pickle

import qualified Type.Customer as Customer

data CustomerInfo = CustomerInfo
  { name :: Customer.Name
  } deriving (Generic, Show, Typeable)

deriveAll ''CustomerInfo "PFCustomerInfo"
type instance PF CustomerInfo = PFCustomerInfo

instance XmlPickler CustomerInfo where xpickle = gxpickle
instance JSONSchema CustomerInfo where schema = gSchema
instance ToJSON     CustomerInfo
instance FromJSON   CustomerInfo
