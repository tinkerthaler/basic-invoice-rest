{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  , TemplateHaskell
  , TypeFamilies
  #-}
module Type.InvoiceError where

import Data.Aeson
import Data.JSON.Schema
import Data.Typeable
import GHC.Generics
import Generics.Regular
import Generics.Regular.XmlPickler
import Rest.Error
import Text.XML.HXT.Arrow.Pickle

data InvoiceError = InvalidTitle | InvalidContent
  deriving (Eq, Generic, Ord, Show, Typeable)

deriveAll ''InvoiceError "PFInvoiceError"
type instance PF InvoiceError = PFInvoiceError

instance XmlPickler InvoiceError where xpickle = gxpickle
instance JSONSchema InvoiceError where schema = gSchema
instance FromJSON   InvoiceError
instance ToJSON     InvoiceError

instance ToResponseCode InvoiceError where
  toResponseCode _ = 400
