module Network.Rpc.Json
  ( Method
  , Params
  , Request(..)
  , Response(..)
  , AffjaxLoggingTransport(..)
  , AffjaxTransport(..)
  , class Transport
  , call
  ) where

import Prelude

import Control.Monad.Aff (Aff, error, throwError)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Argonaut.Core (Json, jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.?), (.??))
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Either (Either(..), either)
import Data.Maybe (maybe)
import Network.HTTP.Affjax (AJAX, URL, post)
import Network.HTTP.StatusCode (StatusCode(..))


class Transport c e | c -> e where
  call :: c -> Request -> Aff e (Response Json)

newtype AffjaxTransport = AffjaxTransport URL
instance affjaxTransport :: Transport AffjaxTransport (ajax :: AJAX | e) where
  call (AffjaxTransport url) req = do
    { status: (StatusCode statusCode), response: body } <- post url $ encodeJson req
    when (statusCode /= 200) do
      throwError $ error $ "JSON RPC call "
        <> (show req)
        <> " failed with HTTP status code = "
        <> show statusCode
    either (throwError <<< error) pure $ decodeJson body

newtype AffjaxLoggingTransport = AffjaxLoggingTransport URL
instance affjaxLoggingTransport :: Transport AffjaxLoggingTransport (ajax :: AJAX, console :: CONSOLE | e) where
  call (AffjaxLoggingTransport url) req = do
    let jsonRequest = encodeJson req
    log $ ">>> " <> show jsonRequest
    { status: (StatusCode statusCode), response: body } <- post url jsonRequest
    log $ "<<< " <> show statusCode <> ": " <> show body
    when (statusCode /= 200) do
      throwError $ error $ "JSON RPC call "
        <> (show req)
        <> " failed with HTTP status code = "
        <> show statusCode
    either (throwError <<< error) pure $ decodeJson body

type Method = String
type Params = Array String

newtype Request = Request { id :: Int
                          , method :: Method
                          , params :: Params
                          }

instance encodeRequest :: EncodeJson Request where
  encodeJson (Request req) =
       "id" := req.id
    ~> "method" := req.method
    ~> "params" := (encodeJson req.params)
    ~> "jsonrpc" := "2.0"
    ~> jsonEmptyObject

instance showRequest :: Show Request where
  show = show <<< encodeJson

data Response a = Result a
                | Error Int String -- TODO: Error codes

instance decodeResponse :: DecodeJson r => DecodeJson (Response r) where
  decodeJson json = do
    obj <- decodeJson json
    res <- obj .?? "result"
    maybe (decodeError obj) (Right <<< Result) res
      where decodeError obj = do
              err <- obj .? "error"
              code <- err .? "code"
              message <- err .? "message"
              pure $ Error code message
