module Concurrent.Channel where

import Prelude

import Control.Monad.List.Trans (ListT, nil, prepend', uncons)
import Control.MonadPlus (class Alt, class Alternative, class MonadPlus, class Plus, alt, empty)
import Data.Decide (class Decide)
import Data.Divide (class Divide)
import Data.Divisible (class Divisible, conquer)
import Data.Either (Either(..))
import Data.Functor.Contravariant (class Contravariant)
import Data.Lazy (defer)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (class Traversable, all, traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.AVar as AVar
import Effect.Aff (Aff, catchError, launchAff_)
import Effect.Aff.AVar (AVar, put, take, kill)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error, error, message, throwException)

-- | An `Output` is a wrapper around a function that sends values to a `Channel`. 
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

-- | Will try to send to the first `Output` and if it is closed,
-- | then to the second.
instance semigroupOutput :: Semigroup (Output a) where
  append o o' = Output $ \x -> do
    result <- send o x
    if result
       then pure true
       else send o' x

instance monoidOutput :: Monoid (Output a) where
  mempty = conquer

-- | An `Input` is a wrapper around a function that receive values from a `Channel`. 
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

-- | Will try to receive from the first `Input` and if it is closed,
-- | then from the second.
instance altInput :: Alt Input where
  alt i i' = Input do
    result <- recv i
    case result of
      Nothing -> recv i'
      just -> pure just

instance plusInput :: Plus Input where
  empty = Input $ pure Nothing

instance alternativeInput :: Alternative Input

instance monadPlusInput :: MonadPlus Input

-- | Same as `Alt`
instance semigroupInput :: Semigroup (Input a) where
  append = alt

instance monoidInput :: Monoid (Input a) where
  mempty = empty

instance monadEffectInput :: MonadEffect Input where
  liftEffect eff = Input $ map Just $ liftEffect eff

instance monadAffInput :: MonadAff Input where
  liftAff aff = Input $ map Just aff

-- | A `Channel` is an abstraction over a var, queue or stream backend and
-- | can be used as a communication channel between async threads.
type Channel o i = {output :: Output o, input :: Input i, close :: Aff Unit}

-- | Receives a value from an `Input`. If the `Input` is closed, the result will
-- | be `Nothing`.
recv :: forall a. Input a -> Aff (Maybe a)
recv = unwrap

-- | Sends a value to an `Output`. Will return `false` if the `Output` is closed
-- | and `true` if not.
send :: forall a. Output a -> a -> Aff Boolean
send = unwrap

-- | Creates a new `Channel` using an `AVar` backend.
newChannel :: forall a. Effect (Channel a a)
newChannel = AVar.empty >>= avarChannel

-- | Creates a new `Channel` with a provided `AVar` as backend.
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

-- | Creates a new `Channel` using the `Input` from the first provided `Channel`
-- | and the `Output` from the second. Closing the new `Channel` will also close
-- | the provided two `Channel`s.
inner :: forall o i o' i'. Channel o i -> Channel o' i' -> Channel o' i
inner c c' = {input: c.input, output: c'.output, close: c.close *> c'.close}

-- | Launches an async thread that sends all values from the `Input` to the `Output`
-- | in the background.
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

-- | Lazily drains an `Input` into a `ListT`. The list will end when the Input is
-- | closed.
recvList :: forall m. MonadAff m => Input ~> ListT m
recvList input = do
  elem <- liftAff $ recv input
  case elem of
    Just e -> prepend' e $ defer \_ -> recvList input
    Nothing -> nil

-- | Sends a `ListT` into an `Output`. Returns `false` if the `Output` was closed
-- | before the entire list was sent.
sendList :: forall m a. MonadAff m => Output a -> ListT m a -> m Boolean
sendList output list = do
  unc <- uncons list
  case unc of
    Just (Tuple x xs) -> do
      result <- liftAff $ send output x
      if result
         then sendList output xs
         else pure false
    Nothing -> pure true

-- | Sends a `Traversable` into an `Output`. Returns `false` if the `Output` was closed
-- | before the entire traversable was sent.
sendTraversable :: forall t a. Traversable t => Output a -> t a -> Aff Boolean
sendTraversable output = pure <<< all identity <=< traverse (send output)
