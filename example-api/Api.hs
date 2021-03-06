-- | The API path hierarchy
module Api where

import Rest.Api

import ApiTypes (BlogApi)
import qualified Api.Invoice              as Invoice
import qualified Api.Customer              as Customer
import qualified Api.Invoice.Comment      as Invoice.Comment
import qualified Api.Test              as Test
import qualified Api.Test.ReservedName as ReservedName

-- | Defines a versioned api
api :: Api BlogApi
api = [(mkVersion 1 0 0, Some1 blog)]

-- _ The entire routing table for v1.0.0 of the blog
blog :: Router BlogApi BlogApi
blog =
  root -/ customer
       -/ invoice --/ comment
       -/ test --/ reservedName
  where
    customer         = route Customer.resource
    invoice         = route Invoice.resource
    comment      = route Invoice.Comment.resource
    test         = route Test.resource
    reservedName = route ReservedName.resource
