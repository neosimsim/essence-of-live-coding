{-# LANGUAGE Arrows #-}

module LiveCoding.Gloss
  ( module X
  , module LiveCoding.Gloss
  ) where

-- base
import Control.Concurrent
import Data.IORef

-- transformers
import Control.Monad.Trans.Writer

-- gloss
import Graphics.Gloss as X
import Graphics.Gloss.Interface.IO.Game

-- essenceoflivecoding
import LiveCoding

-- essenceoflivecoding-gloss
import LiveCoding.Gloss.Debugger as X
import LiveCoding.Gloss.PictureM as X

type GlossCellWorldForeground = (GlossCell, [Event], Picture)

playCellForeground :: GlossCell -> IO ()
playCellForeground cell = playIO (InWindow "Bla" (600, 800) (20, 20)) black stepRate (initialWorld cell) toPicture handleEvent playStep

type GlossCellWorld = (MVar GlossCell, [Event], Picture)

-- TODO Abstract external main loops
playCell :: GlossCell -> IO (MVar GlossCell)
playCell cell = do
  var <- newMVar cell
  forkIO $ playIO (InWindow "Bla" (600, 800) (20, 20)) black stepRate (initialWorld var) toPicture handleEvent playStepMVar
  return var

-- TODO Of course these are general for cells
updateGloss :: MVar GlossCell -> GlossCell -> IO ()
updateGloss var newGlossCell = do
  oldGlossCell <- takeMVar var
  putMVar var $ hotCodeSwapGloss newGlossCell oldGlossCell

hotCodeSwapGloss
  :: Cell m a b
  -> Cell m a b
  -> Cell m a b
hotCodeSwapGloss
  (Cell newState newStep)
  (Cell oldState _)
  = Cell
  { cellState = migrate newState oldState
  , cellStep  = newStep
  }

initialWorld cell = (cell, [], blank)
toPicture (_, _, picture) = return picture
handleEvent event (cell, events, picture) = return (cell, event : events, picture)
playStep _ (cell, events, _) = do
  (picture, cell') <- fmap massageWriterOutput $ runWriterT $ step cell events
  return (cell', [], picture)
playStepMVar _ (var, events, _) = do
  cell <- takeMVar var
  (picture, cell') <- fmap massageWriterOutput $ runWriterT $ step cell events
  putMVar var cell'
  return (var, [], picture)

glossWrap :: GlossCell -> IO (LiveProgram IO)
glossWrap cell = do
  pictureVar <- newMVar blank
  eventRef <- newIORef []
  stepVar <- newMVar 0
  let
    getPicture () = takeMVar pictureVar
    putEvent event () = modifyIORef eventRef $ (event :)
    putStep _ () = putMVar stepVar $ 1 / stepRate
  forkIO $ playIO (InWindow "Bla" (600, 800) (20, 20)) black stepRate () getPicture putEvent putStep
  let
    putPicture = putMVar pictureVar
    getEvents = atomicModifyIORef eventRef $ \events -> ([], events)
    getStep = takeMVar stepVar
  return $ liveCell $ proc _ -> do
    _       <- constM getStep            -< ()
    events  <- constM getEvents          -< ()
    picture <- runPictureM cell -< events
    arrM putPicture                            -< picture