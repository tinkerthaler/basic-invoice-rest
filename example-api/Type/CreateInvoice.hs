{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  , TemplateHaskell
  , TypeFamilies
  #-}
module Type.CreateInvoice where

import Data.Aeson
import Data.JSON.Schema
import Data.Text (Text)
import Data.Typeable
import GHC.Generics
import Generics.Regular
import Generics.Regular.XmlPickler
import Text.XML.HXT.Arrow.Pickle

type Title = Text

data CreateInvoice = CreateInvoice
  { title   :: Title
  , content :: Text
  } deriving (Eq, Generic, Ord, Show, Typeable)

deriveAll ''CreateInvoice "PFCreateInvoice"
type instance PF CreateInvoice = PFCreateInvoice

instance XmlPickler CreateInvoice where xpickle = gxpickle
instance JSONSchema CreateInvoice where schema = gSchema
instance FromJSON   CreateInvoice
instance ToJSON     CreateInvoice
