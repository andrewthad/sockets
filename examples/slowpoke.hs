{-# language LambdaCase #-}
{-# language NumericUnderscores #-}
{-# language OverloadedStrings #-}

{-# OPTIONS_GHC -fforce-recomp -O2 -Wall -Werror #-}

import Control.Concurrent (threadDelay)
import Control.Exception (Exception,throwIO)
import Text.Read (readMaybe)
import System.Environment (getArgs)

import qualified Data.ByteString.Short.Internal as SB
import qualified Data.Bytes as Bytes
import qualified Data.Primitive as PM
import qualified Net.IPv4 as IPv4
import qualified Socket.Stream.IPv4 as SI
import qualified Socket.Stream.Uninterruptible.Bytes as SI

main :: IO ()
main = do
  getArgs >>= \case
    [portStr] -> case readMaybe portStr of
      Nothing -> fail "Argument is not a valid port."
      Just port -> unhandled $ SI.withConnection
        (SI.Peer IPv4.loopback port) throwOnCloseException $ \conn -> do
          let go [] = pure ()
              go (x : xs) = do
                unhandled (SI.send conn (Bytes.fromByteArray x))
                threadDelay 800_000
                go xs
          go gettysburgAddress
    [] -> fail "Expected one argument but got zero."
    _ -> fail "Expected one argument but more than one."

unhandled :: Exception e => IO (Either e a) -> IO a
unhandled action = action >>= either throwIO pure

throwOnCloseException :: Either SI.CloseException () -> () -> IO ()
throwOnCloseException e () = either throwIO pure e

gettysburgAddress :: [PM.ByteArray]
gettysburgAddress =
  fmap (\b -> case SB.toShort b of SB.SBS arr -> PM.ByteArray arr)
    [ "Four score and seven years ago our fathers brought forth on this "
    , "continent, a new nation, conceived in Liberty, and dedicated to "
    , "the proposition that all men are created equal. Now we are engaged "
    , "in a great civil war, testing whether that nation, or any nation so "
    , "conceived and so dedicated, can long endure. We are met on a great "
    , "battle-field of that war. We have come to dedicate a portion of that "
    , "field, as a final resting place for those who here gave their lives "
    , "that that nation might live. It is altogether fitting and proper that "
    , "we should do this."
    ]
