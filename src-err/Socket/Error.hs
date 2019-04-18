module Socket.Error
  ( die
  ) where

die :: String -> IO a
die = fail
