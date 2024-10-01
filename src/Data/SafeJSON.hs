{-|
Module      : Data.SafeJSON
Copyright   : (c) 2019 Felix Paulusma
License     : MIT
Maintainer  : felix.paulusma@gmail.com
Stability   : experimental

Please read the

__[README on GitHub](https://github.com/Vlix/safe-json/blob/v1.1.0/README.md)__

for an extensive explanation of this library, why and how to use it,
and examples.
-}
module Data.SafeJSON
    (
    -- * Conversion to/from versioned JSON
    --
    -- | These functions are the workhorses of the library.
    --
    --   As long as a type has a 'SafeJSON' instance and, if conversion
    --   from other types is required, a 'Migrate' instance, these will
    --   make sure to add and read version numbers, and handle migration.
      safeToJSON
    , safeFromJSON
    , strippedSafeToJSON
    -- * SafeJSON Class
    --
    -- | This class, together with 'Migrate', is where the magic happens!
    --
    --   Using the 'SafeJSON' class to define the form and expected
    --   migration to a type, and defining 'Migrate' instances to describe
    --   how to handle the conversion from older versions (or maybe a
    --   newer version) to the type, you can be sure that your programs
    --   will still parse the JSON of types it is expecting.
    , SafeJSON(version, kind, safeTo, safeFrom, objectProfile, typeName)
    -- ** Contained
    , Contained
    , contain
    -- ** Defining 'safeFrom' and 'safeTo'
    --
    -- | If the type doesn't already have 'Data.Aeson.FromJSON' and
    --   'Data.Aeson.ToJSON' instances, the following functions can help
    --   in defining the 'safeFrom' and 'safeTo' methods.
    --
    -- @
    -- safeFrom = containWithObject \"MyType\" $ \\o ->
    --   MyType \<$\> o .:  "regular_value"
    --          \<*\> o .:$ "safe_value"
    --
    -- safeTo (MyType regular safe) =
    --   contain . object $
    --     [ "regular_value" .=  regular
    --     , "safe_value"    .=$ safe
    --     ]
    -- @

    -- *** Inspecting values in 'safeFrom'
    --
    -- | The following functions are helpful when defining 'safeFrom'.
    --   They are basically 'contain' composed with the corresponding
    --   "Data.Aeson" function, so they can be used in the same fashion
    --   as said "Data.Aeson" function.
    , containWithObject
    , containWithArray
    , containWithText
    , containWithScientific
    , containWithBool
    -- *** Accessors
    --
    -- | These accessors can be used like their "Data.Aeson" counterparts.
    --   The only difference is that the expected value is parsed using
    --   'safeFromJSON' instead of 'Data.Aeson.parseJSON'.
    , (.:$)
    , (.:$?)
    , (.:$!)
    -- *** Constructor for 'safeTo'
    --
    -- | This constructor of key-value pairs can be used exactly like
    --   its "Data.Aeson" counterpart ('Data.Aeson..='), but converts the
    --   given value with 'safeToJSON' instead of 'Data.Aeson.toJSON'
    , (.=$)
    -- ** Version
    --
    -- | All 'SafeJSON' instances have a 'version'. This version will be
    --   attached to the JSON format and used to figure out which parser
    --   (and as such, which type in the chain) should be used to parse
    --   the given JSON.
    , Version
    , noVersion
    , setVersion
    , setVersion'
    , getVersion
    , removeVersion
    -- ** Kind
    --
    -- | All 'SafeJSON' instance have a declared 'kind', indicating if any
    --   migration needs to happen when parsing using 'safeFromJSON'.
    --
    -- * The Base kind (see 'base') is at the bottom of the chain and will
    --   not be migrated to. They can optionally have no version tag by
    --   defining: @'version' = 'noVersion'@.
    --   /N.B./ 'base' /and/ 'extended_base' /are the only kinds that can/
    --   /be paired with/ 'noVersion'.
    --
    -- * Extensions (see 'extension' and 'extended_extension') tell the
    --   system that there exists at least one previous version of the data
    --   type which should be migrated from if needed.
    --   (This requires the data type to also have a @'Migrate' a@ instance)
    --
    -- * Forward extensions (see 'extended_base' and 'extended_extension')
    --   tell the system there exists at least one next version from which
    --   the data type can be reverse-migrated.
    --   (This requires the data type to also have a @'Migrate' ('Reverse' a)@
    --   instance)
    , Kind
    , base
    , extension
    , extended_base
    , extended_extension
    -- *** Showing the type
    --
    -- | These helper functions can be used to easily define 'typeName'.
    --   As long as the type being defined has a 'Typeable' instance.
    , typeName0
    , typeName1
    , typeName2
    , typeName3
    , typeName4
    , typeName5
    -- ** Consistency
    , Profile(..)
    , ProfileVersions(..)
    -- * Migration
    , Migrate(..)
    , Reverse(..)
    ) where

import Data.SafeJSON.Internal
