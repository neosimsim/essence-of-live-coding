{-# LANGUAGE RecordWildCards #-}
module Handle.LiveProgram where

-- base
import Control.Arrow

-- containers
import qualified Data.IntMap as IntMap

-- transformers
import Control.Monad.Trans.RWS.Strict (RWS, tell)
import qualified Control.Monad.Trans.RWS.Strict as RWS
import Control.Monad.Trans.State.Strict

-- test-framework
import Test.Framework

-- test-framework-quickcheck2
import Test.Framework.Providers.QuickCheck2

-- essence-of-live-coding
import LiveCoding
import LiveCoding.Handle
import Util.LiveProgramMigration
import Control.Monad.Trans.Class (MonadTrans(lift))

testHandle :: Handle (RWS () [String] Int) String
testHandle = Handle
  { create = do
      n <- RWS.get
      return $ "Handle #" ++ show n
  , destroy = const $ tell ["Destroyed handle"]
  }

test = testGroup "Handle.LiveProgram"
  [ testProperty "Trigger destructors in live program" LiveProgramMigration
    { liveProgram1 = runHandlingState $ liveCell
        $ handling testHandle >>> arrM (lift . tell . return) >>> constM inspectHandlingState
    , liveProgram2 = runHandlingState mempty
    , input1 = replicate 3 ()
    , input2 = replicate 3 ()
    , output1 = replicate 3 ["Handle #0", "Handles: 1", "Destructors: (1,True)"]
    , output2 = [["Destroyed handle"], [], []]
    , initialState = 0
    }
  ]
    where
      inspectHandlingState = do
        HandlingState { .. } <- get
        lift $ tell
          [ "Handles: " ++ show nHandles
          , "Destructors: " ++ unwords ((show . second isRegistered) <$> IntMap.toList destructors)
          ]
