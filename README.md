# aeson-picker

[![Travis](https://img.shields.io/travis/ozzzzz/aeson-picker.svg)](https://travis-ci.org/ozzzzz/aeson-picker)
[![hackage](https://img.shields.io/hackage/v/aeson-picker.svg)](https://hackage.haskell.org/package/aeson-picker)
[![hackage-deps](https://img.shields.io/hackage-deps/v/aeson-picker.svg)](https://hackage.haskell.org/package/aeson-picker)

Tiny library to get fields from JSON format

Common use is the following:
```
JSON |-- FIELDS :: EXPECTED TYPE
Text |-- [Text] :: a
```
So operator `(|--)` gets JSON (represented as `Text`), "route" to field inside JSON (represented as `[Text]`), and tries to parse field from JSON with described route to expected type.
If expected type can be deduced with type checker then it can be dropped down.

A little bit safer operator is `(|-?)`. It returns not `a` but `Maybe a`.

## Examples
First, add extension (to not pack `String` to `Text` every time):
```
ghci>:set -XOverloadedStrings
```

Then you can try something simple (empty list means that you try to parse `Value` from the top JSON-level):
```
ghci>"5" |-- [] :: Int
5
ghci>"5" |-- [] :: Float
5.0
ghci>"5" |-- [] :: String
"*** Exception: Data.Aeson.Picker: could not pick field with path: []
```

But what if field you are looking for somewhere inside JSON?
That's why are you here.

Let's try to get something from inside JSON:
```
ghci>"{\"a\": 5}" |-- ["a"] :: Int
5
```
But be sure that the field is presented inside JSON:
```
ghci>"{\"a\": 5}" |-- ["b"] :: Int
*** Exception: Data.Aeson.Picker: could not pick field with path: ["b"]
```
We can go deeper (as deep as you want):
```
ghci>"{\"outer\": {\"inner\": [1,2,3]}}" |-- ["outer", "inner"] :: [Int]
[1,2,3]
```
But be sure that you JSON is really valid (by specification key in JSON should be `String`):
```
ghci>"{a: 5}" |-- ["a"] :: Int
*** Exception: Data.Aeson.Picker: input json is not valid
```

If you want more "safe" picker, you can use another operator:
```
ghci>"5" |-? [] :: Maybe Int
Just 5
ghci>"{\"a\": 5}" |-? ["a"] :: Maybe Int
Just 5
ghci>"{\"a\": 5}" |-? ["b"] :: Maybe Int
Nothing
```

In current logic even operator `(|-?)` will throw error if JSON is not valid:
```
ghci>"{a: 5}" |-? ["a"] :: Maybe Int
*** Exception: Data.Aeson.Picker: input json is not valid
```

You can open issue if you do not think that it is right logic.

