{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Twelf.TwelfServer
       ( TwelfMonadT
       , withTwelfServer
       , runTwelfCmd
       ) where

import Control.Monad.Catch
import Control.Monad.Reader

import Data.List

import System.Exit
import System.IO hiding (stdin, stdout)
import System.Process

data TwelfProc = TwelfProc { twelfStdin :: Handle
                           , twelfStdout :: Handle
                           , twelfProc :: ProcessHandle
                           , twelfDebug :: Bool }

newtype TwelfMonadT m a = TwelfMonadT (ReaderT TwelfProc m a)
    deriving (Functor, Applicative, Monad, MonadReader TwelfProc, MonadIO, MonadTrans)

runTwelfCmd :: MonadIO m => String -> TwelfMonadT m String
runTwelfCmd cmd = do
  twelfin  <- asks twelfStdin
  twelfout <- asks twelfStdout
  debug <- asks twelfDebug
  let errmsg = "Twelf subprocess reported an error." ++
               if debug then "" else "\nRerun with --debug to see details."
      getresp = do l <- hGetLine twelfout
                   when debug $ hPutStrLn stderr $ "< " ++ l
                   case l of
                     "%% ABORT %%" -> error errmsg
                     "%% OK %%"    -> return []
                     _ -> (l:) <$> getresp
  liftIO $ hPutStrLn twelfin $ cmd ++ "\n"
  when debug $
    liftIO $ mapM_ (hPutStrLn stderr . ("> "++)) $ lines cmd
  liftIO $ liftM (intercalate "\n") getresp

startTwelfProcess :: MonadIO m => String
                  -> m (Handle, Handle, ProcessHandle)
startTwelfProcess bin = do
  (Just stdin, Just stdout, _, pid) <-
    liftIO $ createProcess $ (proc bin [])
      { std_in    = CreatePipe
      , std_out   = CreatePipe
      , std_err   = CreatePipe
      , close_fds = True }
  code <- liftIO $ getProcessExitCode pid
  case code of
    Just (ExitFailure e) ->
      error $ "cannot start " ++ bin ++ ": error " ++ show e
    _ -> do liftIO $ hSetBuffering stdin NoBuffering
            return (stdin, stdout, pid)

withTwelfServer :: (MonadIO m, MonadMask m) => String -> Bool -> TwelfMonadT m a -> m a
withTwelfServer bin debug m =
  bracket
    (startTwelfProcess bin)
    (\(_, _, pid) -> liftIO $ terminateProcess pid)
    (\(stdin, stdout, pid) ->
      runReaderT m'
        TwelfProc { twelfStdin  = stdin
                  , twelfStdout = stdout
                  , twelfProc   = pid
                  , twelfDebug  = debug })
      where TwelfMonadT m' = runTwelfCmd "" >> m
