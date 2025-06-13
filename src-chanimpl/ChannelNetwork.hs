{-# language BangPatterns #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}

module ChannelNetwork
  ( SendException
  , ReceiveException
  , ConnectException
  , Resource
  , M
  , connect
  , withConnection
  , send
  , receiveExactly
  , showsPrecConnectException
  , showsPrecReceiveException
  , showsPrecSendException
  ) where

import Control.Exception (bracket)
import Data.Bytes.Chunks (Chunks)
import Data.Int (Int32)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Word (Word16)
import Foreign.C.Error (Errno)
import Network.Socket (Socket)
import Network.Unexceptional.ByteArray (receiveExactly)
import Network.Unexceptional.Chunks (send)

import qualified Data.Text as T
import qualified Foreign.C.Error.Describe as Describe
import qualified Network.Socket as N
import qualified Network.Unexceptional as NU

type SendException = Errno
type ReceiveException = Errno
type ConnectException = Errno

showsPrecConnectException :: Int -> ConnectException -> String -> String
showsPrecConnectException = showsPrecErrno

showsPrecSendException :: Int -> SendException -> String -> String
showsPrecSendException = showsPrecErrno

showsPrecReceiveException :: Int -> ReceiveException -> String -> String
showsPrecReceiveException = showsPrecErrno

showsPrecErrno :: Int -> Errno -> String -> String
showsPrecErrno _ e s = Describe.string e ++ (' ' : s)

type M = IO

type Resource = Socket

connect ::
     Text -- hostname
  -> Word16 -- port
  -> M (Either ConnectException Resource)
connect host !port = do
  let hints = N.defaultHints { N.addrSocketType = N.Stream }
  minfo <- N.getAddrInfo (Just hints) (Just (T.unpack host)) (Just (show port))
  info <- case minfo of
    info : _ -> pure info
    [] -> fail "Impossible: getAddrInfo cannot return empty list"
  sock <- N.openSocket info
  NU.connect sock (N.addrAddress info) >>= \case
    Right (_ :: ()) -> pure (Right sock)
    Left e -> pure (Left e)

-- TODO: Make this not throw exceptions when the broker's hostname
-- cannot be resolved or when creating the socket fails.
withConnection ::
     Text -- hostname
  -> Word16 -- port
  -> (Either ConnectException Resource -> M a)
  -> M a
withConnection host !port handler = do
  let hints = N.defaultHints { N.addrSocketType = N.Stream }
  minfo <- N.getAddrInfo (Just hints) (Just (T.unpack host)) (Just (show port))
  info <- case minfo of
    info : _ -> pure info
    [] -> fail "Impossible: getAddrInfo cannot return empty list"
  bracket (N.openSocket info) N.close $ \sock ->
    NU.connect sock (N.addrAddress info) >>= \case
      Right (_ :: ()) -> handler (Right sock)
      Left e -> handler (Left e)
