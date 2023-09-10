# Changelog for safe-json

## 1.2.0.0

* Compatibility with GHC 9.6.* and `tasty < 1.6`
* Removed `SafeJSON a` constraint on `SafeJSON (Product f g a)`
* Added `getVersion` function to check a `Data.Aeson.Value` for a `SafeJSON` version

## 1.1.4.0

* Compatibility with GHC 9.4.*, and `aeson < 2.3`

## 1.1.3.1

* Compatibility with GHC 9.2.*, and `time < 0.13`

## 1.1.3.0

* Compatibility with `aeson < 2.1` and `text < 2.1` [#33](https://github.com/Vlix/safe-json/pull/33) Thanks to [@ysangkok](https://github.com/ysangkok)
* Compatibility with `bytestring < 0.12`.

## 1.1.2.0

* Aeson <= 2.0.2.0 compatibility (https://cs-syd.eu/posts/2021-09-11-json-vulnerability)
    * Fix internal code to work with new `Key` and `KeyMap` from `aeson-2.0.0.0` [#28](https://github.com/Vlix/safe-json/pull/28) Thanks to [@dysinger](https://github.com/dysinger)
    * Added `SafeJSON` instances for `Key` and `KeyMap` [#29](https://github.com/Vlix/safe-json/pull/29)
    * Added `SafeJSON` instances for `Compose`, `Product` and `Sum` [#29](https://github.com/Vlix/safe-json/pull/29)

## 1.1.1.1

* loosened dependecy restriction on `tasty`
* fixed some documentation

## 1.1.1

* Fix clash in `test/Instances.hs` of `Ord` instance for `Data.Aeson.Value` [#23](https://github.com/Vlix/safe-json/pull/23)

## 1.1.0

* update for GHC 8.8.1 [#15](https://github.com/Vlix/safe-json/pull/15)
    * loosened dependency restriction on `time`
    * fixed instance for IntMap
* DRY-er `TestMigrate` and `TestReverseMigrate` type synonyms [#17](https://github.com/Vlix/safe-json/pull/17) Thanks to [@blinkytoy](https://github.com/blinkytoy)
* fixed documentation [#17](https://github.com/Vlix/safe-json/pull/17) Thanks to [@blinkytoy](https://github.com/blinkytoy)
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
