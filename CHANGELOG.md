# Revision history for stream-sockets

## 0.3.1.0

* Add functions for sending strict bytestrings and lazy bytestrings
  over a stream socket (`sendByteArray` and `sendLazyByteArray`).
* Add `receiveByteString` to `Socket.Stream.IPv4`. This is a function
  for receive strict bytestring.
* Add `sendAddr`. This is used in the implementations of both
  `sendByteArray` and `sendLazyByteArray`.

## 0.3.0.0

* Totally redo almost everything. The API is now considered fairly
  stable.

## 0.1.0.0 -- 2019-01-18

* First version. Released on an unsuspecting world.
