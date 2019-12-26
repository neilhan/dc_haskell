module Ch05.Json (JsonValue(..),
             getString,
             getInt,
             getDouble,
             getBool,
             getObject,
             getArray,
             isNull) where

data JsonValue = JsonString String
               | JsonNumber Double
               | JsonBool Bool
               | JsonNull
               | JsonObject [(String, JsonValue)]
               | JsonArray [JsonValue]
                  deriving (Eq, Ord, Show)

data A =
  A {field :: String}
  deriving (Show)

getString :: JsonValue -> Maybe String
getString (JsonString s) = Just s
getString _ = Nothing

getInt (JsonNumber n) = Just (truncate n)
getInt _ = Nothing

getDouble (JsonNumber n) = Just n
getDouble _ = Nothing

getBool (JsonBool b) = Just b
getBool _ = Nothing

getObject (JsonObject o) = Just o
getObject _ = Nothing

getArray (JsonArray a) = Just a
getArray _ = Nothing

isNull v = v == JsonNull
