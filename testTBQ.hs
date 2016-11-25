import Control.Concurrent.STM.TBQueue
import Control.Concurrent.STM
import Control.Concurrent

main = do
  q <- newTBQueueIO 0
  forkIO $ do
    print "hello"
    v <- atomically $ readTBQueue q
    print v
  atomically $ writeTBQueue q "a"

