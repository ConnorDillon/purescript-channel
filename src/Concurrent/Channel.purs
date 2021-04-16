module Concurrent.Channel where

import Prelude

import Control.MonadPlus (class Alt, class Alternative, class MonadPlus, class MonadZero, class Plus, alt, empty)
import Data.Decide (class Decide)
import Data.Divide (class Divide)
import Data.Divisible (class Divisible, conquer)
import Data.Either (Either(..))
import Data.Functor.Contravariant (class Contravariant)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.AVar as AVar
import Effect.Aff (Aff, catchError, launchAff_)
import Effect.Aff.AVar (AVar, put, take, kill)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error, error, message, throwException)

newtype Output a = Output (a -> Aff Boolean)

derive instance newtypeOutput :: Newtype (Output a) _

instance contravariantOutput :: Contravariant Output where
  cmap fn (Output x) = Output $ x <<< fn

instance divideOutput :: Divide Output where
  divide fn o o' = Output $ \x -> let Tuple y z = fn x
    in (||) <$> send o y <*> send o' z

instance divisibleOutput :: Divisible Output where
  conquer = Output (const $ pure false)

instance decideOutput :: Decide Output where
  choose fn o o' = Output $ \x -> case fn x of
    Left y -> send o y
    Right z -> send o' z

instance semigroupOutput :: Semigroup (Output a) where
  append o o' = Output $ \x -> (||) <$> send o x  <*> send o' x

instance monoidOutput :: Monoid (Output a) where
  mempty = conquer

newtype Input a = Input (Aff (Maybe a))

derive instance newtypeInput :: Newtype (Input a) _

derive instance functorChannel :: Functor Input

instance applyInput :: Apply Input where
  apply (Input fn) (Input x) = Input $ apply (map apply fn) x

instance applicativeInput :: Applicative Input where
  pure x = Input $ pure $ pure x

instance bindInput :: Bind Input where
  bind (Input aff) fn = Input $ aff >>= maybe (pure Nothing) (recv <<< fn)

instance monadInput :: Monad Input

instance altInput :: Alt Input where
  alt i i' = Input do
    result <- recv i
    case result of
      Nothing -> recv i'
      just -> pure just

instance plusInput :: Plus Input where
  empty = Input $ pure Nothing

instance alternativeInput :: Alternative Input

instance monadZeroInput :: MonadZero Input

instance monadPlusInput :: MonadPlus Input

instance semigroupInput :: Semigroup (Input a) where
  append = alt

instance monoidInput :: Monoid (Input a) where
  mempty = empty

instance monadEffectInput :: MonadEffect Input where
  liftEffect eff = Input $ map Just $ liftEffect eff

instance monadAffInput :: MonadAff Input where
  liftAff aff = Input $ map Just aff

type Channel o i = {output :: Output o, input :: Input i, close :: Aff Unit}

recv :: forall a. Input a -> Aff (Maybe a)
recv = unwrap

send :: forall a. Output a -> a -> Aff Boolean
send = unwrap

newChannel :: forall a. Effect (Channel a a)
newChannel = AVar.empty >>= avarChannel

avarChannel :: forall a. AVar a -> Effect (Channel a a)
avarChannel var = pure {output: Output trySend, input: Input tryRecv, close}
  where
    msg = "Killed AVar due to closed Channel"
    catch :: forall r. r -> Error -> Aff r
    catch r e = if message e == msg
      then pure r
      else liftEffect $ throwException e
    trySend x = catchError
      (put x var *> pure true)
      (catch false)
    tryRecv = catchError
      (map pure $ take var)
      (catch Nothing)
    close = kill (error msg) var

inner :: forall o i o' i'. Channel o i -> Channel o' i' -> Channel o' i
inner c c' = {input: c.input, output: c'.output, close: c.close *> c'.close}

connect :: forall a. Input a -> Output a -> Effect Unit
connect i o = launchAff_ $ recv i >>= loop
  where
    loop val = case val of
      Just x -> do
        result <- send o x
        if result
           then recv i >>= loop
           else pure unit
      Nothing -> pure unit
