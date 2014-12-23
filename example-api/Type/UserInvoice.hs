{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  , TemplateHaskell
  , TypeFamilies
  #-}
module Type.UserInvoice where

import Data.Aeson
import Data.JSON.Schema
import Data.Typeable
import GHC.Generics
import Generics.Regular
import Generics.Regular.XmlPickler
import Text.XML.HXT.Arrow.Pickle

import Type.CreateInvoice (CreateInvoice)
import Type.User (User)

data UserInvoice = UserInvoice { user :: User, post :: CreateInvoice }
  deriving (Eq, Generic, Ord, Show, Typeable)

deriveAll ''UserInvoice "PFUserInvoice"
type instance PF UserInvoice = PFUserInvoice

instance XmlPickler UserInvoice where xpickle = gxpickle
instance JSONSchema UserInvoice where schema = gSchema
instance FromJSON   UserInvoice
instance ToJSON     UserInvoice
