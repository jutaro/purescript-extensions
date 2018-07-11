-----------------------------------------------------------------------------
--
-- Module      :  Extensions
-- Copyright   :  2015 Jürgen Nicklisch-Franken
-- License     :  GPL 2
--
--
-- | Some extensions for purescript
--
-----------------------------------------------------------------------------

module Extensions where

import Prelude
-- import Control.Monad.Eff (kind Effect, Eff)
import Effect (Effect)
import Data.Array (length, range, unsafeIndex)
import Data.List (List(..))
import Data.Traversable (sequence) --???
import Math (floor)
import Partial.Unsafe (unsafePartial)
import Data.JSDate(JSDate)
-- import Data.CForeign (Foreign)
import Foreign (Foreign)


infixl 2 bindConst as >>

foreign import parseDate :: String -> JSDate

foreign import toISOString :: JSDate -> String

bindConst :: forall m a b. (Bind m) => m a -> m b -> m b
bindConst x y = x >>= const y

-- A range function, which only counts up like in Haskell
hRange :: Int -> Int -> Array Int
hRange a b = if a > b then [] else range a b

-- A real mod function for Ints (not a remainder like % and mod in purescript)
modInt :: Int -> Int -> Int
modInt n d    =  if signum r == negate (signum d) then r+d else r
                    where r = n `mod` d

signum :: Int -> Int
signum n | n > 0 = 1
signum n | n < 0 = -1
signum n | otherwise = 0

-- | A mod function for Floats
modFloat :: Number -> Number -> Number
modFloat n d = n - (floor (n / d)) * d

-- | Perform a monadic action `n` times collecting all of the results.
replicateM :: forall m a. Monad m => Int -> m a -> m (Array a)
replicateM n m
  | n < 1 = pure []
  | otherwise = sequence $ replicate n m

-- | Create a list with repeated instances of a value.
listReplicate :: forall a. Int -> a -> List a
listReplicate n value = go n Nil
  where
  go n' rest | n' <= 0 = rest
             | otherwise = go (n' - 1) (Cons value rest)

-- foreign import data TIMEOUT :: EffectKind
-- foreign import kind EffectKind

foreign import timeout :: forall  a.
                               Int ->
                               Effect a ->
                               Effect Unit

-- Throws an error
foreign import fail :: forall a . String -> a

-- Undefined, which matches any type
foreign import undef :: forall a . a

-- Anything goes
foreign import unsafeCoerce :: forall a b. a -> b

-- | Perform a fold using a monadic step function.
foldM :: forall m a b. Monad m => (a -> b -> m a) -> a -> Array b -> m a
foldM f a array = go a 0
  where
  go res i
    | i >= length array = pure res
    | otherwise = do
        res' <- f res (unsafePartial (unsafeIndex array i))
        go res' (i + 1)

-- Monadic map
mapM :: forall a b m. (Monad m) => (a -> m b) -> Array a -> m (Array b)
mapM f array = sequence (map f array)

-- Logging
data LogLevel =
      Trace
    | Debug
    | Info
    | Warn
    | Error
    | Fatal

logA ::  String -> Effect Unit
logA str = logAny Trace str \_ -> pure unit

log :: forall a. String -> (Unit -> a) -> a
log = logAny Trace

logA' :: LogLevel -> String -> Effect Unit
logA' logLevel str = logAny logLevel str \_ -> pure unit

log' :: forall a. LogLevel -> String -> (Unit -> a) -> a
log' = logAny

-- | Map with effects over an array of values.
foreign import mapE :: forall a b. (a -> Effect b) -> Array a -> Effect (Array b)

-- | Map with effects over an array of values. Doesn't return a value
foreign import mapE_ :: forall a. (a -> Effect Unit) -> Array a -> Effect Unit

-- | Map with effects over an array of values. Calls a break function on every iteration with the index.
-- If the braek function returns true, the computation will be stopped.
foreign import mapEBreak :: forall a b. (a -> Effect b) -> (Int -> Effect Boolean) -> Array a -> Effect (Array b)

-- | Map with effects over an array of values. Calls a break function on every iteration with the index.
-- If the braek function returns true, the computation will be stopped. Doesn't return a value
foreign import mapEBreak_ :: forall a. (a -> Effect Unit) -> (Int -> Effect Boolean) -> Array a -> Effect Unit

-- | Create an array with repeated instances of a value.
foreign import replicate :: forall a. Int -> a -> Array a

-- Should go to: Graphics.Canvas
foreign import data Image :: Type

foreign import alert :: String -> Effect Unit

foreign import logAny :: forall a s. LogLevel -> s -> (Unit -> a) -> a

foreign import stringify :: Foreign -> String

