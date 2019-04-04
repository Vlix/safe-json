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
    -- ** Showing the type
    --
    -- These helper functions can easily be used in the
    -- definition of 'errorTypeName'. As long as the type
    -- being defined has a 'Typeable' instance.
    , typeName
    , typeName1
    , typeName2
    , typeName3
    , typeName4
    , typeName5
    -- * Testing
    , Profile(..)
    , ProfileVersions(..)
    -- * Migration
    , Migrate(..)
    ) where


import Data.SafeJSON.Internal
import Data.SafeJSON.Instances()
