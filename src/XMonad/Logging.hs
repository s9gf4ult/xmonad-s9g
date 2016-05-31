module XMonad.Logging where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.DynamicState
import System.Log.FastLogger
import XMonad.Core
import XMonad.Lens

instance MonadLogger X where
  monadLoggerLog loc source lvl msg = do
    ds <- view $ _config . _payload
    case getDyn ds of
      Nothing -> return ()
      Just logger -> do
        liftIO $ pushLogStr logger $ toLogStr msg
