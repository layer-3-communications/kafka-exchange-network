{-# language KindSignatures #-}

module ChannelNetwork
  ( SendException
  , ReceiveException
  , ConnectException
  , Resource
  , M
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

type SendException = Errno
type ReceiveException = Errno
type ConnectException = ()

showsPrecConnectException :: Int -> ConnectException -> String -> String
showsPrecConnectException = showsPrec

showsPrecSendException :: Int -> SendException -> String -> String
showsPrecSendException = showsPrecErrno

showsPrecReceiveException :: Int -> ReceiveException -> String -> String
showsPrecReceiveException = showsPrecErrno

showsPrecErrno :: Int -> Errno -> String -> String
showsPrecErrno _ e s = Describe.string e ++ (' ' : s)

type M = IO

type Resource = Socket

-- TODO: Make this not throw exceptions.
withConnection ::
     Text -- hostname
  -> Word16 -- port
  -> (Either ConnectException Resource -> M a)
  -> M a
withConnection host port handler = do
  let hints = N.defaultHints { N.addrSocketType = N.Stream }
  minfo <- N.getAddrInfo (Just hints) (Just (T.unpack host)) (Just (show port))
  info <- case minfo of
    info : _ -> pure info
    [] -> fail "Impossible: getAddrInfo cannot return empty list"
  bracket (N.openSocket info) N.close $ \sock -> do
    N.connect sock (N.addrAddress info)
    handler (Right sock)
