{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  , TemplateHaskell
  , TypeFamilies
  #-}
module Type.Invoice where

import Data.Aeson
import Data.JSON.Schema
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Typeable
import GHC.Generics
import Generics.Regular
import Generics.Regular.XmlPickler
import Text.XML.HXT.Arrow.Pickle

import qualified Type.Customer as Customer

type Id = Int
type Title = Text

data Invoice = Invoice
  { id          :: Id
  , author      :: Customer.Name
  , createdTime :: UTCTime
  , title       :: Title
  , content     :: Text
  } deriving (Eq, Generic, Ord, Show, Typeable)

deriveAll ''Invoice "PFInvoice"
type instance PF Invoice = PFInvoice

instance XmlPickler Invoice where xpickle = gxpickle
instance JSONSchema Invoice where schema = gSchema
instance ToJSON     Invoice
instance FromJSON   Invoice

instance XmlPickler UTCTime where xpickle = xpPrim
