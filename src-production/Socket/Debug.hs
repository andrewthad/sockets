module Socket.Debug
  ( debug
  , whenDebugging
  , debugging
  ) where

debug :: String -> IO ()
debug _ = pure ()

whenDebugging :: IO () -> IO ()
whenDebugging _ = pure ()

debugging :: Bool
debugging = True
