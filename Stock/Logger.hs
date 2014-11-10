{-# LANGUAGE OverloadedStrings #-}
module Stock.Logger
       ( execLogger
       , logStd
       , Logger (..)
       ) where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import           System.IO.Unsafe

type Log = String

newtype Logger a = Logger { runLogger :: [Log] -> (a, [Log]) }

----------------------------------------------------------------------
-- *** Monad

instance Monad Logger where
  return a = Logger $ \ls -> (a, ls)
  (Logger a) >>= f = Logger $ \ls ->
    let
      (val, ls') = a ls
    in
     runLogger (f val) ls'

instance Functor Logger where
  fmap f (Logger a) = Logger $ \ls ->
    let (val, ls') = a ls
    in ((f val), ls')

instance Applicative Logger where
  pure a = Logger $ \ls -> (a, ls)
  f <*> a = Logger $ \ls ->
    let (f1, ls') = runLogger f ls
        (val, ls'') = runLogger a ls''
    in ((f1 val), ls'')


----------------------------------------------------------------------
-- *** MonadT

newtype LoggerT m a = LoggerT { runLoggerT :: [Log] -> m (a, [Log])}
instance Monad m => Monad (LoggerT m) where
  return x = LoggerT $ \ls -> return (x, ls)
  m >>= k = LoggerT $ \ls -> do
    let logger = runLoggerT m
    (a, ls') <- logger ls
    let logger' = runLoggerT (k a)
    logger' ls

instance MonadTrans LoggerT where
  lift m = LoggerT $ \ls -> do
    res <- m
    return (res, ls)

instance (MonadIO m) => MonadIO (LoggerT m) where
  liftIO = lift . liftIO
----------------------------------------------------------------------
--

execLogger :: (Monad m, MonadIO m) => LoggerT m a -> m a
execLogger logger = do
  (a, logs) <- runLoggerT logger []
  liftIO $ sequence_ $ map putStrLn logs
  return a

logStd :: (Monad m) => String -> LoggerT m ()
logStd l = LoggerT $ \ls -> return ((), ls ++ [l])

logWarning :: (Monad m) => String -> LoggerT m ()
logWarning l = logStd ("Warning" ++ l)

logError :: (Monad m) => String -> LoggerT m ()
logError l = logStd ("Error: " ++ l)

--logger :: LoggerT m ()
--logger = pure ()

test :: LoggerT IO ()
test = do
  logStd "foo"
  liftIO $ putStrLn "Hello, World"
  logStd "Bar"

t = execLogger test
