module Data.SafeJSON
    ( -- * SafeJSON Instance
      SafeJSON(version, kind, safeTo, safeFrom, objectProfile, typeName)
    , contain
    , safeToJSON
    , safeFromJSON
    , Version
    , noVersion
    , Kind
    , base
    , extension
    , extended_base
    , extended_extension
    -- ** Showing the type
    --
    -- These helper functions can easily be used in the
    -- definition of 'typeName'. As long as the type
    -- being defined has a 'Typeable' instance.
    , typeName0
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
    , Reverse(..)
    ) where


import Data.SafeJSON.Internal
import Data.SafeJSON.Instances()
