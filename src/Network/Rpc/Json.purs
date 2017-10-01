module Network.Rpc.Json
  ( Method
  , Params
  , Error(..)
  , error
  , Request(..)
  , Response(..)
  , AffjaxLoggingTransport(..)
  , AffjaxTransport(..)
  , class Transport
  , call
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Aff as A
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Argonaut.Core (Json, jsonEmptyObject, stringify)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.?), (.??))
import Data.Argonaut.Encode (class EncodeJson, assoc, encodeJson, extend, (:=), (~>))
import Data.Either (Either(..), either)
import Data.Maybe (Maybe, maybe)
import Data.Newtype (class Newtype)
import Network.HTTP.Affjax (AJAX, URL, post)
import Network.HTTP.StatusCode (StatusCode(..))


class Transport c e | c -> e where
  call :: c -> Request -> A.Aff e (Response Json)

newtype AffjaxTransport = AffjaxTransport URL
instance affjaxTransport :: Transport AffjaxTransport (ajax :: AJAX | e) where
  call (AffjaxTransport url) req = do
    { status: (StatusCode statusCode), response: body } <- post url $ encodeJson req
    when (statusCode /= 200) do
      A.throwError $ A.error $ "JSON RPC call "
        <> (show req)
        <> " failed with HTTP status code = "
        <> show statusCode
    either (A.throwError <<< A.error) pure $ decodeJson body

newtype AffjaxLoggingTransport = AffjaxLoggingTransport URL
instance affjaxLoggingTransport :: Transport AffjaxLoggingTransport (ajax :: AJAX, console :: CONSOLE | e) where
  call (AffjaxLoggingTransport url) req = do
    let jsonRequest = encodeJson req
    log $ ">>> " <> show jsonRequest
    { status: (StatusCode statusCode), response: body } <- post url jsonRequest
    log $ "<<< " <> show statusCode <> ": " <> show body
    when (statusCode /= 200) do
      A.throwError $ A.error $ "JSON RPC call "
        <> (show req)
        <> " failed with HTTP status code = "
        <> show statusCode
    either (A.throwError <<< A.error) pure $ decodeJson body

type Method = String
type Params = Array String

newtype Request = Request { id :: Int
                          , method :: Method
                          , params :: Params
                          }

derive instance newtypeRequest :: Newtype Request _

derive instance eqRequest :: Eq Request

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
                      , data :: Maybe Json
                      }

error :: ∀ d.
         EncodeJson d =>
         Int -> String -> Maybe d ->
         Error
error c m d = Error { code: c
                    , message: m
                    , data: encodeJson <$> d
                    }

derive instance newtypeError :: Newtype Error _

derive instance eqError :: Eq Error

instance encodeJsonError :: EncodeJson Error where
  encodeJson (Error e) =
    let cd = "code" := e.code
        ms =  "message" := e.message
        add = assoc "data" >>> flip extend jsonEmptyObject
        tl = maybe jsonEmptyObject add e.data
    in cd ~> ms ~> tl

instance decodeJsonError :: DecodeJson Error where
  decodeJson json = do
    obj  <- decodeJson json
    code <- obj .? "code"
    msg  <- obj .? "message"
    dta  <- obj .?? "data"
    pure $ Error { code: code, message: msg, data: dta }

data Response a = Response Int (Either Error a)

derive instance eqResponse :: Eq a => Eq (Response a)

instance functorResponse :: Functor Response where
  map f (Response id (Right a)) = Response id $ Right (f a)
  map _ (Response id (Left e)) = Response id $ Left e

instance showResponse :: Show a => Show (Response a) where
  show (Response id (Left (Error e))) =
    "Response { id = " <> show id
          <> ", error code = " <> show e.code
          <> ", error message = " <> show e.message
          <> ", error data = " <> show (stringify <$> e.data)
          <> "}"
  show (Response id (Right r)) =
    "Response { id = " <> show id
          <> ", result = " <> show r
          <> "}"

instance encodeResponse :: EncodeJson r =>
                           EncodeJson (Response r) where
  encodeJson (Response id (Right r)) =
      "jsonrpc" := "2.0"
   ~> "id" := id
   ~> "result" := r
   ~> jsonEmptyObject
  encodeJson (Response id (Left  e)) =
       "jsonrpc" := "2.0"
    ~> "id" := id
    ~> "error" := e
    ~> jsonEmptyObject

instance decodeJsonResponse :: DecodeJson r =>
                               DecodeJson (Response r) where
  decodeJson json = do
    o <- decodeJson json
    id <- o .? "id"
    it <- Right <$> (o .? "result" >>= decodeJson)
      <|> Left  <$> (o .? "error"  >>= decodeJson)
    pure $ Response id it
