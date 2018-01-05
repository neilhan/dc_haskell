
module Week3 where

skips :: [a] -> [[a]]
skips xs =
  map (skips_stride xs) [0..(length xs - 1)]
  where
      skips_stride xs' stride =
          if stride < (length xs')
          then (head $ drop stride xs') : skips_stride (drop (stride + 1) xs') stride
          else []
