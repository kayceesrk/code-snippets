{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,UndecidableInstances, FlexibleContexts, TypeSynonymInstances #-}
import Data.Monoid
import Control.Monad
import Control.Monad.Writer
import Control.Monad.Trans
import System.IO
import Data.IORef
import Network
import System (getArgs)

type Action =
  Atom Action
| Fork Action Action
| Stop

type C a = (a -> Action) -> Action

instance Monad C where
   (>>=) ::  C a -> (a -> C b) -> C b
   m >>= f  = \ ( k :: b -> Action) -> m ( \ a -> f a k )

   return :: a -> C a
   return x = \ k -> k x
