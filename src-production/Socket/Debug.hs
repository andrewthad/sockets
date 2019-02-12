module Socket.Debug
  ( debug
  , whenDebugging
  ) where

debug :: String -> IO ()
debug _ = pure ()

whenDebugging :: IO () -> IO ()
whenDebugging _ = pure ()
