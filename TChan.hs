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
  w <- readTVar waiting
  case st of
    Nothing ->
      if w
        then do
          writeTVar ref (Just v)
        else retry
    _ -> retry

recv :: MyChan a -> STM a
recv (ref,waiting) = do
  st <- readTVar ref
  case st of
    Nothing -> do
      writeTVar waiting True
      retry
    Just v -> do
      writeTVar ref Nothing
      writeTVar waiting False
      return v

numIters :: Int
numIters = 1

main :: IO ()
main = do
  c <- newMyChan
  forkIO $ replicateM_ numIters $ atomically $ send c 0
  replicateM_ numIters $ do
    v <- atomically $ recv c
    print v
