{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  , TemplateHaskell
  , TypeFamilies
  #-}
module Type.CustomerSignupError where

import Data.Aeson
import Data.JSON.Schema
import Data.Typeable
import GHC.Generics
import Generics.Regular
import Generics.Regular.XmlPickler
import Rest.Error
import Text.XML.HXT.Arrow.Pickle

data CustomerSignupError = InvalidPassword | InvalidCustomerName
  deriving (Eq, Generic, Ord, Show, Typeable)

deriveAll ''CustomerSignupError "PFCustomerSignupError"
type instance PF CustomerSignupError = PFCustomerSignupError

instance XmlPickler CustomerSignupError where xpickle = gxpickle
instance JSONSchema CustomerSignupError where schema = gSchema
instance FromJSON   CustomerSignupError
instance ToJSON     CustomerSignupError

instance ToResponseCode CustomerSignupError where
  toResponseCode _ = 400
