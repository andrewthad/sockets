module Socket.Debug
  ( debug
  ) where

debug :: String -> IO ()
debug = putStrLn
