{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}
module LiveCoding.Warp
  ( runWarpC
  , module X
  ) where

-- base
import Control.Concurrent
import Control.Monad.IO.Class

-- http-types
import Network.HTTP.Types as X

-- wai
import Network.Wai as X

-- warp
import Network.Wai.Handler.Warp

-- essence-of-live-coding
import LiveCoding

data WaiHandle = WaiHandle
  { requestVar  :: MVar Request
  , responseVar :: MVar Response
  , appThread   :: ThreadId
  }

waiHandle :: Port -> Handle IO WaiHandle
waiHandle port = Handle
  { create = do
      requestVar <- newEmptyMVar
      responseVar <- newEmptyMVar
      let app request respond = do
              putMVar requestVar request
              response <- takeMVar responseVar
              respond response
      appThread <- forkIO $ run port app
      return WaiHandle { .. }
  , destroy = \WaiHandle { .. } -> killThread appThread
  }

-- | Start a WARP application on the given port in a background thread,
--   supply the cell with the requests, and serve the responses.
runWarpC
  :: Port
  -> Cell IO Request Response
  -> Cell (HandlingStateT IO) () ()
runWarpC port cell = proc () -> do
  WaiHandle { .. } <- handling $ waiHandle port -< ()
  request <- arrM $ liftIO . takeMVar           -< requestVar
  response <- liftCell cell                     -< request
  arrM $ liftIO . uncurry putMVar               -< (responseVar, response)
  returnA                                       -< ()
