module Test.Main where

import Prelude

import Concurrent.Channel (connect, newChannel, recv, send)
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

testConnect :: Aff Unit
testConnect = test "connect" do
  chan1 <- liftEffect newChannel
  chan2 <- liftEffect newChannel
  liftEffect $ connect chan1.input chan2.output
  void $ send chan1.output 1
  void $ send chan1.output 2
  result1 <- recv chan2.input
  result2 <- recv chan2.input
  pure $ result1 == Just 1 && result2 == Just 2

main :: Effect Unit
main = launchAff_ do
  testSend
  testSendRecv
  testCloseRecv
  testCloseSend
  testConnect
