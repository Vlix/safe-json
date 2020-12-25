[![Build Status](https://secure.travis-ci.org/Vlix/safe-json.svg?branch=master)](https://travis-ci.org/Vlix/safe-json)
[![Hackage](https://img.shields.io/hackage/v/safe-json.svg)](https://hackage.haskell.org/package/safe-json)
[![Stackage LTS](http://stackage.org/package/safe-json/badge/lts)](http://stackage.org/lts/package/safe-json)
[![Stackage Nightly](http://stackage.org/package/safe-json/badge/nightly)](http://stackage.org/nightly/package/safe-json)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](./LICENSE)

# safe-json

#### Automatic JSON format versioning

This library aims to make the updating of JSON formats or contents,
while keeping backward compatibility, as painless as possible. The
way this is achieved is through versioning and defined migration
functions to migrate older (or newer) versions to the one used.

---

* [Why?](#why)
  * [Some effects of changing a message format](#some-effects-of-changing-a-message-format)
* [How does it work](#how-does-it-work)
  * [SafeJSON](#safejson)
    * [Version](#version)
    * [Kind](#kind)
    * [Type name](#type-name)
    * [`safeFrom` and `safeTo`](#safefrom-and-safeto)
  * [Migrate](#migrate)
    * [Reverse migration](#reverse-migration)
* [Keep in mind](#keep-in-mind)
  * [Testing](#testing)
  * [Using `noVersion`](#using-noversion)
  * [Using `setVersion` and `removeVersion`](#using-setversion-and-removeversion)
  * [Non-object versioning](#non-object-versioning)
* [Examples](#examples)
  * [Fresh start](#fresh-start)
  * [Production migration](#production-migration)
    * [No down-time cheatsheet](#no-down-time-cheatsheet)
* [Acknowledgments](#acknowledgments)

---

## Why?

An obvious example would probably be JSON messages used in a
production environment for communication between (micro-)services.

In a long running setting, there will most likely come a moment
that the JSON message in question will need to be restructured,
updated, or otherwise changed. This can result in some mild or more
serious headaches, depending on the architecture of the communicating
services.

### Some effects of changing a message format

* If the messages are being logged (e.g. to a database) and are used
in any way in the normal operation of the services, this results in
"old format" messages either needing to be excluded from queries after
the change to the new format, or all messages needing to be migrated
over to the new format manually.

* If downtime is undesirable (which is often the case), then an
in-place update of services will cause "new format" messages to be
received by the "old format" expecting services, and vice-versa.
This might be countered with creating new endpoints for the new
format and making sure the messages are routed correctly, etc. etc.
But this still includes overhead when the semantics of the endpoints
didn't actually change.

Why can't you just update the message format and leave everything as
is? That is the goal of this library.

## How does it work

The library mainly consists of two classes:

* [`SafeJSON a`](#safejson): Defines the version of `a` and if (and how) it is
migratable.
* [`Migrate a`](#migrate): Defines the data type (`MigrateFrom a`) that can be
migrated to `a` and how to migrate from that type.

### SafeJSON

The `SafeJSON` class defines the following:

* [`version`](#version): a unique identifier
* [`kind`](#kind): if and how to migrate to this type
* [`typeName`](#type-name): string to identify the type
* [`safeFrom`](safefrom-and-safeto): how to parse _from_ a JSON value
* [`safeTo`](safefrom-and-safeto): how to parse _to_ a JSON value

The default implementations of those last two use `parseJSON` and
`toJSON`, so if you already have `FromJSON` and `ToJSON` instances
for your type, you don't have to define them.
(There are some exceptions: read [Version](#version) / [safeFrom/-To](#safefrom-and-safeto))

__`safeFrom` and `safeTo` can not be used directly! The functions
that implement the versioning and migrating are `safeFromJSON` and
`safeToJSON`, which use `safeFrom` and `safeTo` internally.__

So given that your type already has `FromJSON` and `ToJSON` instances
(and is `Typeable`), the most basic definition of a `SafeJSON` instance
would be the following:

```haskell
instance SafeJSON MyType where
  version = 0
  kind = base
```

This will add the `version` tag as `0` and indicates this is the
first/bottom type in the migration chain.

---

#### Version

The version can be set using integer literals. The only requirement
is that no two types in a chain share the same version number.
(It is only used as a unique identifier.)

The implication is that, when using `safeToJSON`, the resulting JSON
will have an additional version field. How it's added depends on
the format.

If the resulting JSON is an object (which is the most likely), an extra
field will be added called `"!v"` with the number as the value.

```json
{
  "type": "my custom type",
  "someValues": [{},{"testing":true}],
  "!v": 1
}
```

If the resulting JSON is not an object, it will be wrapped in the
following 2-field object:

```json
{
  "~v": 2,
  "~d": "my non-object type"
}
```

The fields (`"!v", "~v" and "~d"`) are chosen to be the least likely
used in any conventional setting, and as such are least likely to
clash with any existing JSON formats. `safeFromJSON` depends on
these fields to recover the version number, so any potential clashes
should be avoided.
(If required, this can be accomplished by adjusting the
[`safeFrom` and `safeTo` methods](#safefrom-and-safeto))

_It is possible to [omit a version tag](#using-noversion), this is
not advised, but might be needed for integrating types that have
been used before using `SafeJSON` and therefore have no version field._

#### Kind

There are four different `kind`s a type can be:

* `base`: This type will not be migrated to when parsing.
  This can be seen as the bottom of the migration chain.
* `extension`: This type has at least one older version it can
  migrate from: the type defined as the `MigrateFrom` in the
  [`Migrate`](#migrate) instance.
* `extended_base`: This type has at least one newer version it
  can reverse migrate from, and none it can regularly migrate from:
  this newer type is defined as `MigrateFrom (Reverse a)`.
  (cf. [Reverse migration](#reverse-migration))
* `extended_extension`: This type has at least one newer and
  one older version it can migrate from. (cf. [Migrate](#migrate)
  and [Reverse migration](#reverse-migration))

A chain of `extension`s makes the backward compatibility work. If
a type is trying to be parsed using `safeFromJSON`, all older version
numbers will be able to be parsed, and subsequently migrated to the
current version.

If a type is also `extended_*`, that means the next/future version
will be tried before going down the chain. This is convenient when
old programs might receive the new type before being phased out,
while still being able to handle the data coming in.

#### Type name

The name of the type is used in the reporting of errors and when
making the `objectProfile`. A set of pre-made functions can be
used to easily define this method. (`typeName0-5`)

#### `safeFrom` and `safeTo`

If the type already has `FromJSON` and `ToJSON` instances,
the default definition will just use those. But if you're only
going to use the `SafeJSON` variants, the parsing from and to JSON
can be defined in the `safeFrom` and `safeTo` methods.
There might also be times when it is preferable to have the `SafeJSON`
parsing be different from the `From-/ToJSON` parsing.

While using `safeFromJSON` in a `parseJSON` definition is completely
valid, it can be desirable to only have versioned sub-parsing
(parsing of versioned values inside other values) happen when
using the `SafeJSON` interface. In those cases, you would have
to define the `safeFrom` and `safeTo` methods. (and using
`safeFromJSON` and `safeToJSON` in those definitions where
appropriate)

_When defining `safeFrom` and `safeTo`, you need to use the
`contain` function._

Since version `1.0.0`, a few convenience functions have been
added to make defining `safeFrom` and `safeTo` methods a lot
easier, and to make the experience more similar to defining
`parseJSON` and `toJSON`. (e.g. `containWithObject`, `.:$`, `.=$`, etc.)

---

### Migrate

The `Migrate` class is where the magic happens; here you define
which type can be converted into which other type.

When defining a migration from an older type to a newer type,
it's as easy as defining:

```haskell
data OldType = OldType Text
data NewType = NewType [Text]

instance Migrate NewType where
  type MigrateFrom NewType = OldType
  migrate (OldType txt) = NewType [txt]
```

Now, whenever JSON is encountered that should be parsed as an
`OldType`, we can parse it as such, and then immediately migrate
it to `NewType`, which is the one the program actually uses.

_Do not forget to set the `kind` of `NewType` to either `extension`
or `extended_extension` to make use of this type of migration._

#### Reverse migration

There is also the option to support a migration from one version
higher up in the chain to the current version. This is done
by defining the `kind` of the type in the `SafeJSON` instance
as one of the `extended_*` kinds and defining a `Migrate
(Reverse a)` instance for the current type. In that case, a
definition might look something like this:

```haskell
-- (using the above data definitions of OldType and NewType)
instance Migrate (Reverse OldType) where
   type MigrateFrom (Reverse OldType) = NewType
   migrate (NewType [])    = Reverse $ OldType ""
   migrate (NewType (t:_)) = Reverse $ OldType t
```

_N.B.: At the moment there is no support for extended migrating
from newer versions further than the one directly defined in
the type's reverse migrate instance, i.e. if the parsing of
the type defined in type `a`'s `MigrateFrom (Reverse a)` fails,
the other attempts will go down the chain, not further up._

## Keep in mind

Here are some points to take note of when using this library.

### Testing

The module `Data.SafeJSON.Test` contains very useful testing functions
to ensure your types and instances are consistent. It is advised to
at least `testConsistency` of any type you create a `SafeJSON` instance
for. This makes sure you don't have any inconsistencies in your
migration chain, which would result in failed parsing of your type(s).

### Using `noVersion`

There is a way to omit the version tag. It's by using `noVersion` instead
of an integer literal when defining the `version` method. This is used
to not add a version tag to 'primitive' values (`Int`, `Text`, `[]`, etc.),
and also to give the possibility of adding a version-less format to a
migration chain.

When switching to `SafeJSON`, you might already have a JSON format in use.
Adding this type as the bottom of a chain is as easy as:

```haskell
instance SafeJSON VersionLessType where
  version = noVersion
  kind = extended_base -- or 'base', if you don't need the forward migration
```

If you include a 'noVersion' (vNil) in your chain, it is advised to remove the
need to include it as soon as possible; since, if the JSON being parsed has no
version (because it might be a completely different message), having a
`noVersion` type in your chain alone will make it try to parse it as such. In
some cases this might lead to a succesful parse, even though it's a completely
different JSON message. For that reason, it is advised to remove the vNil from
your chain as soon as possible.

As long as there is a version number in the JSON, though, vNil will not be
attempted to be parsed, since "a version" doesn't match "no version".

### Using `setVersion` and `removeVersion`

These functions are new in `safe-json-1.0.0`, since the `FromJSON` and `ToJSON`
constraints have been dropped from `SafeJSON`'s definition and thus it is not
guaranteed you can use `Data.Aeson` functions to handle versionless JSON `Value`s.

_CAUTION: Use these functions at your own risk!
It is always best to use versioning if possible!_

* `setVersion` will insert/override the given type's version in the given JSON `Value`.
* `removeVersion` will remove all the `SafeJSON` versioning from the JSON `Value`

#### `setVersion`

Of course, you'd always like to have the correct version present in your `Value`s,
but sometimes this is not appropriate or desireable. One example would be when
parsing incoming JSON from third parties (e.g. customers), which you don't want
to impose the `SafeJSON` versioning onto.

Be warned this does only set the version at the top-level! `setVersion` does
not recursively set versions! (i.e. if your `safeFrom` definition, for example,
uses `safeFromJSON` when parsing certain fields, these fields will not get the
correct version from using `setVersion` on the overall JSON `Value`)

In these cases, it is recommended to use a `FromJSON` instance (which doesn't
use `safeFromJSON` in its definition) for your incoming type, instead of `SafeJSON`.
You can still use `SafeJSON` internally, because if the type has a `FromJSON`
instance, the `SafeJSON` instance can just use that implementation for the
`safeFrom` definition.

This way you are guaranteed the only difference between `safeFromJSON` and
`parseJSON` is the requirement of a version field in `safeFromJSON`'s case
and it removes the need to use `setVersion` (which is preferable).

```haskell
GIVEN:

data MyType
instance SafeJSON MyType where
  version = 0

-- toJSON adds no version fields
incomingJSON = toJSON [MyType, MyType]
```

```haskell
WRONG: this will not parse using SafeJSON functions.

λ> encode $ setVersion @MyType incomingJSON
{
  "~v": 0,
  "~d": [
    array_of_unversioned_MyTypes
  ]
}
```

```haskell
RIGHT: This will parse using SafeJSON functions.

λ> Just vals = parseMaybe safeFromJSON/parseJSON incomingJSON :: Maybe [Value]
λ> encode $ setVersion @MyType <$> vals
[
  {
    "my_type_field1": xxx,
    "my_type_field2": xxx,
    "!v": 0
  },
  {
    "my_type_field1": xxx,
    "my_type_field2": xxx,
    "!v": 0
  },
]
```

#### `removeVersion`

Conversely, `removeVersion` should, ideally, only be used when the JSON is
leaving your application or platform and you don't want the `SafeJSON`
versioning to be visible to the outside world.

`removeVersion`, on the other hand, __does__ remove all version fields
recursively, so `removeVersion . safeToJSON` will produce a JSON `Value`
with all `"!v"`, `"~v"` and `"~d"` fields removed.

### Non-object versioning

As described in [Version](#version), the version tag added to a non-object
Value has more relative overhead than the tag added to a JSON object.
(min. 14 bytes and min. 7 bytes, respectively)

To keep general overhead low, it is advised to version your entire message,
and only version individual fields if necessary.

# Examples

I want to give two example use cases for `SafeJSON`. The first is a [fresh start](#fresh-start)
with `SafeJSON` and how you can migrate different versions at once. The second
is starting from a JSON format without versioning and then migrating into
`SafeJSON` in a production setting.

## Fresh start

_This is an arbitrary example; some things might seem contrived._

The data types we're working with:

```haskell
data FirstType = FirstType Text
  deriving (Eq, Show)

data SecondType = SecondType (Text, Maybe Int)
  deriving (Eq, Show)

data ThirdType = ThirdType {
  ttFirstName :: Text
  ttLastName :: Text,
  ttAge :: Int,
} deriving (Eq, Show)
```

_`FromJSON` and `ToJSON` instances are included at the bottom of this example._

##### Backstory

We've started using our program with `FirstType`, and that went well for a
couple of weeks. Then we wanted to add a field to maybe include the age of
whatever the data was (if it was something that has an age), this is the
`SecondType`. A while after, we noticed we only had data on people, so why
not represent it as such. (`ThirdType`)

JSON formats with versions added:

```json
// FirstType
{
  "type": "myType",
  "data": "Johnny Doe",
  "!v": 0
}
```

```json
// SecondType
{
  "type": "myType",
  "name": "Johnny Doe",
  "age": 27,
  "!v": 1
}
```

```json
// ThirdType
{
  "type": "myType",
  "firstName": "Johnny",
  "lastName": "Doe",
  "age": 27,
  "!v": 2
}
```

---

SafeJSON instances:

```haskell
-- This sets 'version' to '0', and 'kind' to 'base'
instance SafeJSON FirstType
```

```haskell
instance SafeJSON SecondType where
  version = 1
  kind = extension

instance Migrate SecondType where
  type MigrateFrom SecondType = FirstType
  migrate (FirstType name) = SecondType (name,Nothing)
```

```haskell
import qualified Data.Char as C
import qualified Data.Text as T

instance SafeJSON Thirdtype where
  version = 2
  kind = extension

instance Migrate ThirdType where
  type MigrateFrom ThirdType = SecondType
  migrate (SecondType (name, mAge)) = ThirdType {
      ttFirstName = firstName,
      ttLastName  = lastName,
      ttAge       = fromMaybe (-1) mAge
    }
    where (firstName,rest) = T.break C.isSpace name
          lastName = T.dropWhile C.isSpace rest
```

---

Our database in which we saved our data in JSON now has
three different JSON formats, depending on how far back in
time we go. But that's no problem for us! We just request
all the JSON from the database, which result in a `[Value]`,
and if we want to use it immediately, just use something like
the following:

```haskell
parseValues :: [Value] -> Either String [ThirdType]
parseValues = mapM $ parseEither safeFromJSON
```

But we can also use the `[Value]` in the response body of a
HTTP request when requested by a different program, and that
program can just use a function from the `Data.Aeson.Safe`
module, like `eitherDecode`, to parse the `ByteString` body:

```haskell
import Data.Aeson.Safe as Safe

foo = do
  res <- httpLbs theRequest
  let eVal = Safe.eitherDecode $ responseBody res :: [ThirdType]
  case eVal of
    Left err  -> putStrLn $ "bad value in response: " ++ err
    Right tts -> withAllVersionsToThird tts
```

The HTTP response would maybe look something like this:

```json
[
  {
    "type": "myType",
    "data": "Johnny Doe",
    "!v": 0
  },
  {
    "type": "myType",
    "name": "Jonathan Doe",
    "age": null,
    "!v": 1
  },
  {
    "type": "myType",
    "name": "Shelley Doegan",
    "age": 27,
    "!v": 1
  },
  {
    "type": "myType",
    "firstName": "Anita",
    "lastName": "McDoe",
    "age": 26,
    "!v": 2
  }
]
```

Which would result in the following Haskell data:

```haskell
[ ThirdType {ttFirstName = "Johnny"  , ttLastName = "Doe"   , ttAge = -1}
, Thirdtype {ttFirstName = "Jonathan", ttLastName = "Doe"   , ttAge = -1}
, Thirdtype {ttFirstName = "Shelley" , ttLastName = "Doegan", ttAge = 27}
, Thirdtype {ttFirstName = "Anita"   , ttLastName = "McDoe" , ttAge = 26}
]
```

#### FromJSON and ToJSON instances

```haskell
instance ToJSON FirstType where
  toJSON (FirstType txt) = object
      [ "type" .= String "myType"
      , "data" .= txt
      ]

instance FromJSON FirstType where
  parseJSON = withObject "FirstType" $ \o -> do
      typ  <- o .: "type"
      guard $ typ == String "myType"
      val <- o .: "data"
      return $ FirstType val
```

```haskell
instance ToJSON SecondType where
  toJSON (SecondType (name, age)) = object
      [ "type" .= String "myType"
      , "name" .= name
      , "age"  .= age
      ]

instance FromJSON SecondType where
  parseJSON = withObject "SecondType" $ \o -> do
      typ  <- o .: "type"
      guard $ typ == String "myType"
      name <- o .: "name"
      age  <- o .:? "age"
      return $ SecondType (name, age)
```

```haskell
{-# LANGUAGE RecordWildCards #-}

instance ToJSON ThirdType where
  toJSON ThirdType{..} = object
      [ "type" .= String "myType"
      , "firstName" .= ttFirstName
      , "lastName"  .= ttLastName
      , "age"       .= ttAge
      ]

instance FromJSON ThirdType where
  parseJSON = withObject "ThirdType" $ \o -> do
      typ  <- o .: "type"
      guard $ typ == String "myType"
      ttFirstName <- o .: "firstName"
      ttLastName  <- o .: "lastName"
      ttAge       <- o .: "age"
      return Thirdtype{..}
```

## Production migration

_This is an arbitrary example; some things might seem contrived._

We've started using JSON (without versioning) as a messaging
format between live services. Adding to it has been easy, but
we've hit a point where we need to change the format in a way
that current services would not be able to parse them.

<!-----------------
  start of MyType
------------------->

The data type already in production:

```json
{
  "id": "00000000-0000-0000-0000-000000000000",
  "command": "add_user",
  "person": {
    "firstName": "John",
    "middleName": null,
    "lastName": "Doe"
  },
  "age": 45,
  "address": {
    "street": "Steenstraat",
    "number": "25",
    "addition": "A",
    "city": "Koekel",
    "country": "Friesland"
  },
  "phoneNumber": null
}
```

```haskell
data Message = Message {
  mId :: UUID,
  mCommand :: Text,
  mPerson :: Person,
  mAge :: Int,
  mAddress :: Address,
  mPhoneNumber :: Maybe PhoneNumber
} deriving (Eq, Show)
```

_`FromJSON` and `ToJSON` instances are included at the bottom of this example._

---

The format we want to change to:

```json
{
  "!v": 0,
  "id": "00000000-0000-0000-0000-000000000000",
  "command": "add_user",
  "data": {
    "person": {
      "firstName": "John",
      "middleName": null,
      "lastName": "Doe"
    },
    "age": 45,
    "address": {
      "street": "Steenstraat",
      "number": "25",
      "addition": "A",
      "city": "Koekel",
      "country": "Friesland"
    },
    "phoneNumber": null
  }
}
```

We'll represent this as the first versioned type of this message.

```haskell
data Message_v0 = Message_v0 {
  msgId :: UUID,
  msgCommand :: Text,
  msgData :: PersonalInfo
} deriving (Eq, Show)

data PersonalInfo = PersonalInfo {
  piPerson :: Person,
  piAge :: Int,
  piAddress :: Address,
  piPhoneNumber :: Maybe PhoneNumber
} deriving (Eq, Show)
```

We'll then create the `SafeJSON` and corresponding `Migrate`
instances:

```haskell
{-# LANGUAGE RecordWildCards #-}

instance SafeJSON Message where
  -- | This is important, since our old type has no version tag
  version = noVersion
  -- | extended_* makes sure we can migrate from the newer version
  --   back to this one, since the newer formats will start going
  --   through the system the moment the new services are deployed
  --   and we want the older services to keep functioning.
  kind = extended_base

instance Migrate (Reverse Message) where
  type MigrateFrom (Reverse Message) = Message_v0
  migrate Message_v0{..} = Message
      msgId
      msgCommand
      piPerson
      piAge
      piAddress
      piPhoneNumber
    where PersonalInfo{..} = msgData

instance SafeJSON Message_v0 where
  version = 0
  -- | 'extension' ensures the new services will be able to
  --   handle any old formats still floating around.
  kind = extension

instance Migrate Message_v0 where
  type MigrateFrom Message_v0 = Message
  migrate Message{..} =
      Message_v0 mId mCommand person
    where person = PersonalInfo
              mPerson
              mAge
              mAddress
              mPhoneNumber
```

---

Assuming we've added `Message_v0` on a new development branch, and
modified the business logic to use `Message_v0`, at this point we
create a temporary branch from the production branch (probably
`master`) and let's call it something like `master-message-migration`.

On this branch, we only add `Message_v0` with the `SafeJSON` and
`Migrate` instances. And replace the JSON functions with `SafeJSON`
ones for anywhere `Message` is received by or sent to the current
services. (i.e. use `decode`/`encode` from `Data.Aeson.Safe` instead
of `Data.Aeson`, or `safeFromJSON`/`safeToJSON` instead of
`parseJSON`/`toJSON`)

Then we update the current services so they're ready to use `SafeJSON`
for migrating the new JSON formats we're expecting. After the services
that have `SafeJSON` implemented are the only ones running, we can roll
out the new format without fear of anything throwing parsing errors.

After that, we rename `Message` to `Message_old` and all usages of
`Message_v0` to `Message` and we're back to using `Message` in
our code, but now with a new structure. All while not having to
worry about rerouting messages or down-time.

#### FromJSON and ToJSON instances

```haskell
{-# LANGUAGE RecordWildCards #-}

instance ToJSON Message where
  toJSON Message{..} = object
      [ "id"          .= mId
      , "command"     .= mCommand
      , "person"      .= mPerson
      , "age"         .= mAge
      , "address"     .= mAddress
      , "phoneNumber" .= mPhoneNumber
      ]

instance FromJSON Message where
  parseJSON = withObject "Message" $ \o -> do
    mId      <- o .: "id"
    mCommand <- o .: "command"
    mPerson  <- o .: "person"
    mAge     <- o .: "age"
    mAddress <- o .: "address"
    mPhoneNumber <- o .:? "phoneNumber"
    return Message{..}

instance ToJSON Message_v0 where
  toJSON Message_v0{..} = object
      [ "id"      .= msgId
      , "command" .= msgCommand
      , "data"    .= msgData
      ]

instance FromJSON Message_v0 where
  parseJSON = withObject "Message_v0" $ \o ->
      msgId      <- o .: "id"
      msgCommand <- o .: "command"
      msgData    <- o .: "data"
      return Message_v0{..}

instance ToJSON PersonalInfo where
  toJSON PersonalInfo{..} = object
      [ "person"      .= piPerson
      , "age"         .= piAge
      , "address"     .= piAddress
      , "phoneNumber" .= piPhoneNumber
      ]

instance FromJSON PersonalInfo where
  parseJSON = withObject "PersonalInfo" $ \o ->
      piPerson      <- o .: "person"
      piAge         <- o .: "age"
      piAddress     <- o .: "address"
      piPhoneNumber <- o .: "phoneNumber"
      return PersonalInfo{..}
```

---

### No down-time cheatsheet

From not using `SafeJSON` to using `SafeJSON`:

* Add `SafeJSON` instance to `MyType`.
    * _OPTIONAL: use `noVersion` when previous JSON is already used
      in production_
* Switch `Data.Aeson` functions for the `Data.Aeson.Safe` ones
    * Preferably everywhere in the codebase, unless explicitly
      needed for other purposes.
* __At this point everything should still work like before.__

Create a new development branch to keep the following changes
separate from what's running on the servers. Do the following on
the new branch:

* Rename `MyType` to `MyType_old`:
    * type definition
    * `FromJSON`/`ToJSON` instances
    * `SafeJSON` instance
* Add the type with the new JSON representation
    * name it `MyType`
    * And it's `FromJSON`/`ToJSON`/`SafeJSON` instances
* __At this point you can change your business logic to use the new type.__
* change `kind` methods of both `SafeJSON` instances to make both
  types migrate from eachother.
    * `kind` of `MyType_old`: `extended_base`
    * `kind` of `MyType`: `extension`
* Define `Migrate` instances for both types:
    * `Migrate (Reverse MyType_old)`
    * `Migrate MyType`
* __At this point you have your new updated code ready for use.__

Copy the definition of the `MyType`s with their `*JSON` and
`Migrate` instances to the original branch and overwrite the type
definition and instances of the original `MyType`. Do the following
only on the code you just copied.

* Rename `MyType` to `MyType_v0` (or `MyType_new` or something)
* Rename `MyType_old` to `MyType`.
* __At this point everything should still work like before.__

Now you should have two branches. One with the code that's still
running on your servers (to which we added the new type, its
instances and the migration instances to migrate between the two),
and one branch with the new code that will use the new type.

* Use the original branch to update your running services to
  make them ready to migrate from the new JSON formats.
* After all services are using the new `SafeJSON` code, update
  your services with the code on the new branch.

Enjoy a migration where all services keep parsing all JSON they
receive.

---

# Acknowledgments

The core of this library is inspired by the `safecopy` library
by David Himmelstrup and Felipe Lessa, found on
[GitHub](https://github.com/acid-state/safecopy),
[Hackage](https://hackage.haskell.org/package/safecopy) and
[Stackage](https://www.stackage.org/package/safecopy)
