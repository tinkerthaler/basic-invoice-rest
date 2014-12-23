{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  , TemplateHaskell
  , TypeFamilies
  #-}
module Type.Customer where

import Data.Aeson
import Data.JSON.Schema
import Data.Text (Text)
import Data.Typeable
import GHC.Generics
import Generics.Regular
import Generics.Regular.XmlPickler
import Text.XML.HXT.Arrow.Pickle

type Name = Text
type Password = Text

data Customer = Customer
  { name     :: Name
  , password :: Password
  } deriving (Eq, Generic, Ord, Show, Typeable)

deriveAll ''Customer "PFCustomer"
type instance PF Customer = PFCustomer

instance XmlPickler Customer where xpickle = gxpickle
instance JSONSchema Customer where schema = gSchema
instance FromJSON   Customer
instance ToJSON     Customer
-- We might want to skip the ToJSON instance so we don't accidentally
-- serve passwords, but this type is accepted on signup which means a
-- haskell client needs to be able to serialize it.
