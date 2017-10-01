module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson, (:=), (~>))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Network.Rpc.Json (Error(Error), Response(Response))
import Test.Unit (suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

main :: ∀ e. Eff ( console    :: CONSOLE
                 , testOutput :: TESTOUTPUT
                 , avar       :: AVAR
                 , random     :: RANDOM
                 | e
                 ) Unit
main = runTest do
  let resultResponse = Response 1 $ Right "result"
      resultJson = "id" := 1
                ~> "jsonrpc" := "2.0"
                ~> "result" := "result"
                ~> jsonEmptyObject
      error = Error { code: 42
                    , message: "error"
                    , data: Nothing
                    }
      errorResponse :: Response String
      errorResponse = Response 1 $ Left $ error
      errorJson = "id" := 1
                ~> "jsonrpc" := "2.0"
                ~> "error" := (
                     "code" := 42
                  ~> "message" := "error"
                  ~> jsonEmptyObject
                )
                ~> jsonEmptyObject
  suite "encode" do

    test "encode result response" $
      resultJson `equal` encodeJson resultResponse

    test "encode error response" $
      errorJson `equal` encodeJson errorResponse

  suite "decode" do

    test "decode result response" $
      pure resultResponse `equal` decodeJson resultJson

    test "decode error response" $
      pure errorResponse `equal` decodeJson errorJson
