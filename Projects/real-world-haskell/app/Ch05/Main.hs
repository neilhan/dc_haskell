module Ch05.Main where

import Ch05.Json

main = print (JsonObject [("foo", JsonNumber 1), ("bar", JsonBool False)])
