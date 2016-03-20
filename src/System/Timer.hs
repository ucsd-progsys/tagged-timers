-- | Utilities for timing various IO actions and grouping the times by
--   dynamically generate labels.

module System.Timer
  ( -- * Abstract Datatype
    Timer

    -- * Create a Timer
  , create

    -- * Time an action
  , time

    -- * Show results
  , result
  ) where

import           Control.Monad.IO.Class
import           Data.IORef
import qualified Data.HashMap.Strict as M
import           Data.Time.Clock
-- import           System.CPUTime


-- | A @Timer@ is a (reference to) a map from @Tag@ to a @Double@
--   representing the total time associated with that @Tag@.

data Timer = Timer {tRef :: IORef (M.HashMap Tag Time) }

type Tag   = String
type Time  = NominalDiffTime

--------------------------------------------------------------------------------
-- | Create a new timer
--------------------------------------------------------------------------------
create :: (MonadIO m) => m Timer
--------------------------------------------------------------------------------
create = liftIO $ Timer <$> newIORef M.empty

--------------------------------------------------------------------------------
-- | Time a single action
--------------------------------------------------------------------------------
time :: (MonadIO m) => Timer -> Tag -> m a -> m a
--------------------------------------------------------------------------------
time timer tag act = do
  (x, t) <- timeAct act
  liftIO $  update timer tag t
  return x

update :: Timer -> Tag -> Time -> IO ()
update timer tag n = modifyIORef (tRef timer) bumpTime
  where
    bumpTime m     = M.insert tag (n + M.lookupDefault 0 tag m) m

timeAct :: (MonadIO m) => m a -> m (a, Time)
timeAct act = do
  begin <- liftIO getCurrentTime
  res   <- act
  end   <- liftIO getCurrentTime
  return   (res, diffUTCTime end begin)

--------------------------------------------------------------------------------
-- | Output current results
--------------------------------------------------------------------------------
result :: (MonadIO m) => Timer -> m [(Tag, Time)]
result timer = liftIO $ M.toList <$> readIORef (tRef timer)
