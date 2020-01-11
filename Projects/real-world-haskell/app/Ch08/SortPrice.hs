module Ch08.SortPrice where

import qualified Data.ByteString.Lazy.Char8 as LC

closingPrice = readPrice . (!!4) . LC.split ','
