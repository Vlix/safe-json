# safe-json (Automatic JSON format versioning)

This library aims to make the updating of JSON formats or contents,
while keeping backward compatibility, as painless as possible. The
way this is achieved is through versioning and defined migration
functions to migrate older (or newer) versions to the one used.

*skip to [How does it work](#how-does-it-work) for the actual implementation.*

## Why?

An obvious example would probably be JSON messages used in a
production environment for communication between (micro-)services.
In a long running setting, there will most likely come a moment
that the JSON message in question will need to be restructured,
updated, or otherwise changed. This can result in some mild or more
serious headaches, depending on the architecture of the communicating
services.

### Effect of changing a message format

* If the messages are being logged (e.g. to a database) and are used
in any way in the normal operation of the services, this results in
"old format" messages either needing to be excluded from queries after
the change to the new format, or all messages needing to be migrated
over to the new format manually.

* If downtime is undesireable (which is often the case), then an
in-place update of services will cause "new format" messages to be
received by the "old format" expecting services, and vice-versa.
This might be countered with creating new endpoints for the new
format and making sure the messages are routed correctly, etc. etc.
But this still includes overhead when the semantics of the endpoints
didn't actually change. Why can't you just update the message format
and leave everything as is?

## How does it work

The library mainly consists of two classes:

* `SafeJSON a`: Defines the version of `a` and if (and how) it is
migratable.
* `Migrate a`: Defines the data type (`MigrateFrom a`) that can be
migrated to `a` and how to migrate from that type to `a`.

### SafeJSON

The `SafeJSON` class defines a `version`, the `kind` and the
ways to serialize the type from and to an JSON `Value`; these are
the `safeFrom` and `safeTo` methods. The default implementations
use `parseJSON` and `toJSON`, so you probably don't have to change
these last two. <!-- FIXME: add link to the exception (safeJSON inside safeJSON) -->
I'd also advise to define `typeName`; the `typeName#` functions are
convenient if your type is `Typeable`.

So given that your type already has `FromJSON` and `ToJSON` instances,
and is `Typeable`, the most basic definition of a `SafeJSON` instance
would be the following:

```haskell
instance SafeJSON MyType where
  version = 0
  kind = base
```

This will add the `version` tag as `0` and indicates this is the
first/bottom type in the migration chain.

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

Now, whenever a JSON is encountered that should be parsed as an
`OldType`, we can parse it as such, and then immediately migrate
it to `NewType`, which is the one the program actually uses.



<!--
## Integrating unversioned formats
Explain how Version Nothing works.
And the version fields

Note version fields are  "!v"
and "~v" / "~d"

------------------------
  N.B. about noVersion
------------------------

If you include a 'noVersion' in your chain (vNil), it is advised to remove
the need to include it as soon as possible, or, at least, to make sure no
program tries to parse the JSON as vNil; since, unlike versioned
types, anything trying to still parse the vNil type of your chain, will
ignore the version field and might succeed to parse newer versions if
the 'parseJSON/safeFrom' implementation of vNil would allow it.


### Safe non-object values
Making SafeJSON instances for non-Object 'Value's
creates additional overhead (since they get turned into objects)
so it is advised to try to make SafeJSON instances only for
top-level types that contain other types.


## Defining a SafeJSON instance
While the minimal definition doesn't need any declarations,
it is advised to at least set the 'version' and 'kind'.
(and the 'typeName' if your type is not Typeable)


## explain extended_* kinds
SafeJSON will look forward once, but after that go down the chain.



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

## Examples


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

# Special Thanks

The core of this library is inspired by the `safecopy` library
by David Himmelstrup and Felipe Lessa, found on
[GitHub](https://github.com/acid-state/safecopy),
[Hackage](https://hackage.haskell.org/package/safecopy) and
[Stackage](https://www.stackage.org/package/safecopy)