{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  , TemplateHaskell
  , TypeFamilies
  #-}
module Type.CustomerComment where

import Data.Aeson
import Data.JSON.Schema
import Data.Text (Text)
import Data.Typeable
import GHC.Generics
import Generics.Regular
import Generics.Regular.XmlPickler
import Text.XML.HXT.Arrow.Pickle

import Type.Customer (Customer)

data CustomerComment = CustomerComment
  { customer    :: Customer
  , comment :: Text
  } deriving (Eq, Generic, Ord, Show, Typeable)

deriveAll ''CustomerComment "PFCustomerComment"
type instance PF CustomerComment = PFCustomerComment

instance XmlPickler CustomerComment where xpickle = gxpickle
instance JSONSchema CustomerComment where schema = gSchema
instance FromJSON   CustomerComment
instance ToJSON     CustomerComment
