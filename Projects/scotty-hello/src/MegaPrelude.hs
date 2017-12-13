-- this is to export Prelude
-- can do this:
-- {-# LANGUAGE NoImplicitPrelude #-}
-- import MegaCorpPrelude

module MegaPrelude (
  module Exports,
) where

import Data.Int as Exports
import Data.Tuple as Exports
import Data.Maybe as Exports
import Data.String as Exports
import Data.Foldable as Exports
import Data.Traversable as Exports
import Control.Monad.Trans.Except as Exports
  -- (ExceptT(ExceptT), Except, except, runExcept, runExceptT,
  -- mapExcept, mapExceptT, withExcept, withExceptT)

-- IO
putStr :: MonadIO m => Text -> m ()
putStr = liftIO . Data.Text.IO.putStr

putStrLn :: MonadIO m => Text -> m ()
putStrLn = liftIO . Data.Text.IO.putStrLn

print :: (MonadIO m, Show a) => a -> m ()
print = liftIO . Prelude.print

-- todo ?????? dealing with string convert
-- string-convert, string-conv lib has these

