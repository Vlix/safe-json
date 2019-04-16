# safe-json [![Build Status](https://travis-ci.org/Vlix/safe-json.svg?branch=master)](https://travis-ci.org/Vlix/safe-json)

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
  * [Using `noVersion`](#using-noversion)
  * [Non-object versioning](#non-object-versioning)
* [Examples](#examples)
  * [Fresh start](#fresh-start)
  * [Production migration](#production-migration)
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

* `SafeJSON a`: Defines the version of `a` and if (and how) it is
migratable.
* `Migrate a`: Defines the data type (`MigrateFrom a`) that can be
migrated to `a` and how to migrate from that type.

### SafeJSON

The `SafeJSON` class defines the following:

* [`version`](#version): a unique identifier
* [`kind`](#kind): if and how to migrate to this type
* [`typeName`](#type-name): string to identify the type
* [`safeFrom`](safefrom-and-safeto): how to parse _from_ a JSON value
* [`safeTo`](safefrom-and-safeto): how to parse _to_ a JSON value

The default implementations of those last two use `parseJSON` and
`toJSON`, so you probably don't have to define them.
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

If the resulting JSON is not an object, it will be wrapped in one
with the following fields:

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
been used before using `SafeJSON`._

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

In general, these methods should not have to be defined, since
`FromJSON` and `ToJSON` are constraints on the `SafeJSON` class.
There might be times when it is preferable to have the `SafeJSON`
parsing be different from the `From-/ToJSON` parsing, though.

While using `safeFromJSON` in a `parseJSON` definition is completely
valid, it can be desirable to only have versioned sub-parsing
(parsing of versioned values inside other values) happen when
using the `SafeJSON` interface. In those cases, you would have
to define the `safeFrom` and `safeTo` methods. (and using
`safeFromJSON` and `safeToJSON` in those definitions where
appropriate)

_When defining `safeFrom` and `safeTo`, you need to use the
`contain` function._

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

_N.B. At the moment there is no support for extended migrating
from newer versions further than the one directly defined in
the type's reverse migrate instance, i.e. if the parsing of
the type defined in type `a`'s `MigrateFrom (Reverse a)` fails,
the other attempts will go down the chain, not further up._

## Keep in mind

Here are some points to take note of when using this library.

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

### Non-object versioning

As described in [Version](#version), the version tag added to a non-object
Value has more relative overhead than the tag added to a JSON object.
(min. 14 bytes and min. 7 bytes, respectively)

To keep general overhead low, it is advised to version your entire message,
and only version individual fields if necessary.

## Examples

I want to give two example use cases for `SafeJSON`. The first is a [fresh start](#fresh-start)
with `SafeJSON` and how you can migrate different versions at once. The second
is starting from a JSON format without versioning and then migrating into
`SafeJSON` in a production setting.

### Fresh start

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

---

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

_The FromJSON and ToJSON instances are included at the bottom
of this example._

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
[ ThirdType {ttFirstName = "Johnny", ttLastName = "Doe", ttAge = -1}
, Thirdtype {ttFirstName = "Jonathan", ttLastName = "Doe", ttAge = -1}
, Thirdtype {ttFirstName = "Shelley", ttLastName = "Doegan", ttAge = 27}
, Thirdtype {ttFirstName = "Anita", ttLastName = "McDoe", ttAge = 26}
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
      data <- o .: "data"
      return $ FirstType data
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

### Production migration

<!--
-------------------
  start of MyType
-------------------

data MyType

--------------------
  update to MyType
--------------------

data MyType_new

instance SafeJSON MyType
  version = noVersion
  kind = extended_base

instance SafeJSON MyType_new
  kind = extension

instance Migrate MyType_new
  type MigrateFrom MyType_new = MyType

instance Migrate (Reverse MyType)
  type MigrateFrom (Reverse MyType) = MyType_new

----------------------
  switching in-place
----------------------

In production, good idea to keep branch with only
the instance updates, while working to make the
new formats work.
Then when the new formats are tested -> update only the
instances on the services in production.
When those are all updated and running, update again
with the new format and functionality.

--------------------------
  update usages of types
--------------------------

MyType     -> MyType_old
MyType_new -> MyType


---------------------------
  When to actually change
  'safeFrom' and 'safeTo'
---------------------------

These use 'parseJSON' and 'toJSON' by default, but can be changed
in case the parsing with versioning should be different than without.
This might be the case if not just the entire Value, but also individual
fields in the object should use 'safeFromJSON' or 'safeToJSON', and
you want/need to keep the `FromJSON`/`ToJSON` instances completely
seperate from the `SafeJSON` instance.
-->


```haskell
data MyType = MyType {myA :: Text, myB :: [Int]}

instance FromJSON MyType where
  parseJSON = withObject "MyType" $ \o -> do
      myA <- o .: "text"
      myB <- o .:? "ints" .!= []
      return MyTypes{..}

instance ToJSON MyType where
  toJSON (MyType a b) = object
      [ "text" .= a
      , "ints" .= b
      ]
```

# Acknowledgments

The core of this library is inspired by the `safecopy` library
by David Himmelstrup and Felipe Lessa, found on
[GitHub](https://github.com/acid-state/safecopy),
[Hackage](https://hackage.haskell.org/package/safecopy) and
[Stackage](https://www.stackage.org/package/safecopy)
