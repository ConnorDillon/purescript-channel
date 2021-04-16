module Test.Main where

import Prelude

import Concurrent.Channel (newChannel, recv, send)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Test.Assert (assert')

test :: String -> Aff Boolean -> Aff Unit
test s k = k >>= \r -> liftEffect $ assert' s r *> log ("[OK] " <> s)

testSend :: Aff Unit
testSend = test "send" do
  chan <- liftEffect newChannel
  send chan.output 1

testSendRecv :: Aff Unit
testSendRecv = test "send/recv" do
  chan <- liftEffect newChannel
  void $ send chan.output 1
  result <- recv chan.input
  pure $ result == Just 1

testCloseRecv :: Aff Unit
testCloseRecv = test "close/recv" do
  chan <- liftEffect newChannel
  chan.close
  result <- recv chan.input :: Aff (Maybe Int)
  pure $ result == Nothing

testCloseSend :: Aff Unit
testCloseSend = test "close/send" do
  chan <- liftEffect newChannel
  chan.close
  not <$> send chan.output 1

main :: Effect Unit
main = launchAff_ do
  testSend
  testSendRecv
  testCloseRecv
  testCloseSend
