module Main where
import Debug.Trace
import Control.Exception.Base
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

type MyChan a = (TVar (Maybe a), TVar Bool)

newMyChan :: IO (MyChan a)
newMyChan = do
  ref <- newTVarIO Nothing
  waiting <- newTVarIO False
  return (ref, waiting)

send :: MyChan a -> a -> STM ()
send (ref,waiting) v = do
  st <- readTVar ref
  case st of
    Nothing -> do
      writeTVar ref (Just v)
      w <- readTVar waiting
      when w $ writeTVar waiting False
    _ -> retry

recv :: MyChan a -> STM a
recv (ref,waiting) = do
  w <- readTVar waiting
  if w
    then retry
    else do
      st <- readTVar ref
      case st of
        Nothing -> do
          writeTVar waiting True
          retry
        Just v -> do
          assert (not w) (return ())
          writeTVar ref Nothing
          return v

numIters :: Int
numIters = 10

main :: IO ()
main = do
  c <- newMyChan
  forkIO $ replicateM_ numIters $ do
    v <- atomically $ recv c
    print v
  replicateM_ numIters $ atomically $ send c 0
