module Data.SafeJSON
    ( -- * SafeJSON Instance
      SafeJSON(version, kind, safeTo, safeFrom, objectProfile, errorTypeName)
    , contain
    , safeToJSON
    , safeFromJSON
    , Version
    , Kind
    , primitive
    , base
    , extension
    -- * Testing
    , Profile(..)
    , ProfileVersions(..)
    -- * Migration
    , Migrate(..)
    ) where


import Data.Aeson
import Data.Proxy
import Data.SafeJSON.Internal
import Data.SafeJSON.Instances()
import Data.Typeable
