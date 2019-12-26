module Ch05.PrettyJson (renderJsonValue, Doc) where

import Prelude hiding ((<>))
import Numeric (showHex)
import Data.Bits (shiftR, (.&.))
import Data.Char (ord)

import Ch05.Json (JsonValue(..))

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
           deriving (Show, Eq)


(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y = x `Concat` y


enclose :: Char -> Char -> Doc -> Doc
enclose left right x = char left <> x <> char right


hcat :: [Doc] -> Doc
hcat = fold (<>)


fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty


oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
              Just r -> text r
              Nothing | mustEscape c -> hexEscape c
                      | otherwise -> char c
    where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'


simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
                where ch a b = (a, ['\\', b])


smallHex :: Int -> Doc
smallHex x = text "\\u"
             <> text (replicate (4 - length h) '0')
             <> text h
    where h = showHex x ""


astral :: Int -> Doc
astral n = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
    where a = (n `shiftR` 10) .&. 0x3ff
          b = n .&. 0x3ff


hexEscape :: Char -> Doc
hexEscape c | d < 0x10000 = smallHex d
            | otherwise = astral (d - 0x10000)
    where d = ord c


series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close item = enclose open close . fsep . punctuate (char ',') . map item


fsep :: [Doc] -> Doc
fsep = fold (</>)

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group line

group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line = Char ' '
flatten (x `Union` _) = flatten x
flatten other = other


punctuate :: Doc -> [Doc] -> [Doc]
punctuate p [] = []
punctuate p [d] = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds


empty :: Doc
empty = Empty


char :: Char -> Doc
char c = Char c


text :: String -> Doc
text "" = Empty
text s = Text s


double :: Double -> Doc
double d = text (show d)

line :: Doc
line = Line

string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar


renderJsonValue :: JsonValue -> Doc
renderJsonValue (JsonBool True) = text "true"
renderJsonValue (JsonBool False) = text "false"
renderJsonValue JsonNull = text "null"
renderJsonValue (JsonNumber num) = double num
renderJsonValue (JsonString str) = string str
renderJsonValue (JsonArray ary) = series '[' ']' renderJsonValue ary
renderJsonValue (JsonObject o) = series '{' '}' field o
                                 where field (name, val) = string name <> text ": " <> renderJsonValue val

compact :: Doc -> String
compact x = transform [x]
            where transform [] = ""
                  transform (d:ds) =
                    case d of
                      Empty -> transform ds
                      Char c -> c : transform ds
                      Text s -> s ++ transform ds
                      Line -> '\n' : transform ds
                      a `Concat` b -> transform (a:b:ds)
                      _ `Union` b -> transform (b:ds)

{-|
To try this out in ghci:
stack exec -- ghci -i./app:./src
:l app/Ch05/PrettyJson.hs
import Ch05.PrettyJson
d = JsonObject [("a", JsonString "abc"), ("b", JsonString "bcd")]
putStrLn (compact . renderJsonValue $ d)
-}
