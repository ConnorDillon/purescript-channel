module Test.Main where

import Prelude

import Concurrent.Channel (avarChannel, connect, newChannel, recv, recvList, send, sendList, sendTraversable)
import Control.Monad.List.Trans (foldl, repeat, take)
import Control.Monad.Rec.Class (forever)
import Data.Divide (divided)
import Data.Divisible (conquer)
import Data.Maybe (Maybe(..), isNothing)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.AVar as AVar
import Effect.Aff (Aff, forkAff, joinFiber, launchAff_)
import Effect.Aff.AVar (tryTake)
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

testOutputDivisible :: Aff Unit
testOutputDivisible = test "output/divisible" do
  chan1 <- liftEffect newChannel
  chan2 <- liftEffect newChannel
  let output = divided conquer $ divided chan1.output chan2.output
  result1 <- send output $ Tuple 0 $ Tuple 1 2
  result2 <- recv chan1.input
  result3 <- recv chan2.input
  pure $ result1 && result2 == Just 1 && result3 == Just 2

testOutputMonoid :: Aff Unit
testOutputMonoid = test "output/monoid" do
  var <- liftEffect AVar.empty
  chan1 <- liftEffect newChannel
  chan2 <- liftEffect $ avarChannel var
  result1 <- send (mempty <> chan1.output <> chan2.output) 1
  result2 <- tryTake var
  result3 <- recv chan1.input
  pure $ result1 && isNothing result2 && result3 == Just 1

testInputMonoid :: Aff Unit
testInputMonoid = test "input/monoid" do
  chan1 <- liftEffect $ AVar.new 1 >>= avarChannel
  chan2 <- liftEffect $ AVar.new 2 >>= avarChannel
  let input = mempty <> chan1.input <> chan2.input
  result1 <- recv input 
  chan1.close
  result2 <- recv input
  pure $ result1 == Just 1 && result2 == Just 2

testInputApplicative :: Aff Unit
testInputApplicative = test "input/applicative" do
  chan1 <- liftEffect $ AVar.new 1 >>= avarChannel
  chan2 <- liftEffect $ AVar.new 2 >>= avarChannel
  result1 <- recv $ pure (+) <*> chan1.input <*> chan2.input
  pure $ result1 == Just 3

testInputMonad :: Aff Unit
testInputMonad = test "input/monad" do
  chan1 <- liftEffect $ AVar.new 1 >>= avarChannel
  result1 <- recv $ chan1.input >>= \x -> pure $ x + 1
  pure $ result1 == Just 2

testRecvList :: Aff Unit
testRecvList = test "recv/list" do
  chan <- liftEffect newChannel
  void $ forkAff $ forever $ send chan.output 1
  let list = take 5 $ recvList chan.input
  result <- foldl (+) 0 list
  pure $ result == 5

testSendList :: Aff Unit
testSendList = test "send/list" do
  chan <- liftEffect newChannel
  fiber <- forkAff $ sendList chan.output $ repeat 1
  result1 <- recv $ (+) <$> chan.input <*> chan.input 
  chan.close
  result2 <- joinFiber fiber
  pure $ result1 == Just 2 && not result2

testSendTraversable :: Aff Unit
testSendTraversable = test "send/traversable" do
  chan <- liftEffect newChannel
  fiber <- forkAff $ sendTraversable chan.output [1, 2, 3, 4]
  result1 <- recv $ (+) <$> chan.input <*> chan.input 
  chan.close
  result2 <- joinFiber fiber
  pure $ result1 == Just 3 && not result2

main :: Effect Unit
main = launchAff_ do
  testSend
  testSendRecv
  testCloseRecv
  testCloseSend
  testConnect
  testOutputDivisible
  testOutputMonoid
  testInputMonoid
  testInputApplicative
  testInputMonad
  testRecvList
  testSendList
  testSendTraversable
