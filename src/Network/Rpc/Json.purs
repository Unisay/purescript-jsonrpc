module Network.Rpc.Json
  ( Method
  , Params
  , Request
  , Response(..)
  , AffjaxTransport(..)
  , class Transport
  , call
  ) where

import Prelude
import Control.Monad.Aff (Aff, error, throwError)
import Data.Argonaut.Core (Json, JObject, jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.?), (.??))
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Either (Either(..), either)
import Data.Maybe (maybe)
import Network.HTTP.Affjax (AJAX, URL, post)
import Network.HTTP.StatusCode (StatusCode(..))

newtype AffjaxTransport = AffjaxTransport URL

class Transport c e | c -> e where
  call :: c -> Request -> Aff e (Response Json)

instance affjaxTransport :: Transport AffjaxTransport (ajax :: AJAX | e) where
  call (AffjaxTransport url) req = do
    { status: (StatusCode statusCode), response: body } <- post url $ encodeJson req
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

newtype Error = Error { code :: Int
                      , message :: String
                      }

newtype Response a = Response (Either Error a)

instance functorResponse :: Functor Response where
  map f (Response (Right a)) = Response $ Right (f a)
  map _ (Response (Left e)) = Response $ Left e

instance applyResponse :: Apply Response where
  apply (Response (Right f)) (Response e) = Response (apply f e)

instance applicativeResponse :: Applicative Response where
  pure = Response <<< pure

instance decodeResponse :: DecodeJson r => DecodeJson (Response r) where
  decodeJson json = do
    obj <- decodeJson json
    res <- obj .?? "result"
    maybe (decodeError obj) (pure <<< pure) res
      where
        decodeError :: JObject -> Either String (Response r)
        decodeError o = do
          err <- o .? "error"
          code <- err .? "code"
          message <- err .? "message"
          let error = Error { code, message }
          pure $ Response (Left error)
