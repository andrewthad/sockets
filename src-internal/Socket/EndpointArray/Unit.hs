module Socket.EndpointArray.Unit
  ( EndpointArray
  , MutableEndpointArray
  , new
  , empty
  , write
  , resize
  , freeze
  ) where 

type EndpointArray = ()
type MutableEndpointArray = ()

new :: Int -> IO MutableEndpointArray
new _ = pure ()

empty :: EndpointArray
empty = ()

write :: MutableEndpointArray -> Int -> () -> IO ()
write _ _ _ = pure ()

resize :: MutableEndpointArray -> Int -> IO MutableEndpointArray
resize _ _ = pure ()

freeze :: MutableEndpointArray -> IO EndpointArray
freeze _ = pure ()
