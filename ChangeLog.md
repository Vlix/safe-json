# Changelog for safe-json

## 1.1.2.0

* Aeson 2.0.0.0+ compatibility (https://cs-syd.eu/posts/2021-09-11-json-vulnerability)
    * Fix internal code to work with new `Key` and `KeyMap` from `aeson-2.0.0.0` [#28] Thanks to @dysinger
    * Added `SafeJSON` instances for `Key` and `KeyMap`
    * Added `SafeJSON` instances for `Compose`, `Product` and `Sum`

## 1.1.1.1

* loosened dependecy restriction on `tasty`
* fixed some documentation

## 1.1.1

* Fix clash in `test/Instances.hs` of `Ord` instance for `Data.Aeson.Value` [#23]

## 1.1.0

* update for GHC 8.8.1 [#15]
    * loosened dependency restriction on `time`
    * fixed instance for IntMap
* DRY-er `TestMigrate` and `TestReverseMigrate` type synonyms [#17] Thanks to @blinkytoy
* fixed documentation [#17] Thanks to @blinkytoy
    * broken links to modules
    * `setVersion`'s documentation only showing half

## 1.0.0

* Removed `FromJSON`/`ToJSON` dependecy on `SafeJSON`
    * Default implementation of `safeFrom` and `safeTo` unchanged, still require `FromJSON` and `ToJSON`
* Added unsafe `setVersion` and `removeVersion` functions.
* Integrated `Data.SafeJSON.Instances` into `Data.SafeJSON.Internal`
* Some documentation cleanup/fixes
* Added convenience functions for defining `safeFrom` and `safeTo`
    * e.g. `containWithObject`, `(.:$)`, `(.=$)`, etc.

## 0.1.0

* First release. Includes:
    * `Data.Aeson.Safe`
    * `Data.SafeJSON`
    * `Data.SafeJSON.Instances`
    * `Data.SafeJSON.Internal`
    * `Data.SafeJSON.Test`
