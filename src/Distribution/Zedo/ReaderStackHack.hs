{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module Distribution.Zedo.ReaderStackHack where

import Control.Monad.Reader


instance {-# OVERLAPS #-} (MonadReader a m) => MonadReader a (ReaderT b m) where
    ask = lift ask
    local f mx = do
        b <- ask
        lift $ local f $ runReaderT mx b
