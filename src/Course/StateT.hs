{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Course.StateT where

import Course.Core
import Course.ExactlyOne
import Course.Optional
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad
import Course.State
import qualified Data.Set as S
import qualified Prelude as P

-- $setup
-- >>> import Test.QuickCheck
-- >>> import qualified Prelude as P(fmap)
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary

-- | A `StateT` is a function from a state value `s` to a functor k of (a produced value `a`, and a resulting state `s`).
newtype StateT s k a =
  StateT {
    runStateT ::
      s
      -> k (a, s)
  }

-- | Implement the `Functor` instance for @StateT s k@ given a @Functor k@.
--
-- >>> runStateT ((+1) <$> (pure 2) :: StateT Int List Int) 0
-- [(3,0)]
instance Functor k => Functor (StateT s k) where
  (<$>) ::
    (a -> b)
    -> StateT s k a
    -> StateT s k b
  f <$> s = StateT $ (first f <$>) . runStateT s

-- | Implement the `Applicative` instance for @StateT s k@ given a @Monad k@.
--
-- >>> runStateT (pure 2) 0
-- (2,0)
--
-- >>> runStateT ((pure 2) :: StateT Int List Int) 0
-- [(2,0)]
--
-- >>> runStateT (pure (+2) <*> ((pure 2) :: StateT Int List Int)) 0
-- [(4,0)]
--
-- >>> runStateT (StateT (\s -> Full ((+2), s ++ (1:.Nil))) <*> (StateT (\s -> Full (2, s ++ (2:.Nil))))) (0:.Nil)
-- Full (4,[0,1,2])
--
-- >>> runStateT (StateT (\s -> ((+2), s ++ (1:.Nil)) :. ((+3), s ++ (1:.Nil)) :. Nil) <*> (StateT (\s -> (2, s ++ (2:.Nil)) :. Nil))) (0:.Nil)
-- [(4,[0,1,2]),(5,[0,1,2])]
instance Monad k => Applicative (StateT s k) where
  pure ::
    a
    -> StateT s k a
  pure a = StateT $ pure . (a,)
  (<*>) ::
    StateT s k (a -> b)
    -> StateT s k a
    -> StateT s k b
  sab <*> sa = StateT $ \s -> do
    (f, t) <- runStateT sab s
    (a, u) <- runStateT sa t
    pure (f a, u)

-- | Implement the `Monad` instance for @StateT s k@ given a @Monad k@.
-- Make sure the state value is passed through in `bind`.
--
-- >>> runStateT ((const $ putT 2) =<< putT 1) 0
-- ((),2)
--
-- >>> let modify f = StateT (\s -> pure ((), f s)) in runStateT (modify (+1) >>= \() -> modify (*2)) 7
-- ((),16)
instance Monad k => Monad (StateT s k) where
  (=<<) ::
    (a -> StateT s k b)
    -> StateT s k a
    -> StateT s k b
  f =<< sa = StateT $ \s -> runStateT sa s >>= \(a, t) -> runStateT (f a) t

-- | A `State'` is `StateT` specialised to the `ExactlyOne` functor.
type State' s a =
  StateT s ExactlyOne a

-- | Provide a constructor for `State'` values
--
-- >>> runStateT (state' $ runState $ put 1) 0
-- ExactlyOne ((),1)
state' ::
  (s -> (a, s))
  -> State' s a
state' f = StateT $ ExactlyOne . f

-- | Provide an unwrapper for `State'` values.
--
-- >>> runState' (state' $ runState $ put 1) 0
-- ((),1)
runState' ::
  State' s a
  -> s
  -> (a, s)
runState' sa = runExactlyOne . runStateT sa

-- | Run the `StateT` seeded with `s` and retrieve the resulting state.
--
-- >>> execT (StateT $ \s -> Full ((), s + 1)) 2
-- Full 3
execT ::
  Functor k =>
  StateT s k a
  -> s
  -> k s
execT sa = (snd <$>) . runStateT sa

-- | Run the `State'` seeded with `s` and retrieve the resulting state.
--
-- >>> exec' (state' $ \s -> ((), s + 1)) 2
-- 3
exec' ::
  State' s a
  -> s
  -> s
exec' sa = snd . runState' sa

-- | Run the `StateT` seeded with `s` and retrieve the resulting value.
--
-- >>> evalT (StateT $ \s -> Full (even s, s + 1)) 2
-- Full True
evalT ::
  Functor k =>
  StateT s k a
  -> s
  -> k a
evalT sa = (fst <$>) . runStateT sa

-- | Run the `State'` seeded with `s` and retrieve the resulting value.
--
-- >>> eval' (state' $ \s -> (even s, s + 1)) 5
-- False
eval' ::
  State' s a
  -> s
  -> a
eval' sa = fst . runState' sa

-- | A `StateT` where the state also distributes into the produced value.
--
-- >>> (runStateT (getT :: StateT Int List Int) 3)
-- [(3,3)]
getT ::
  Applicative k =>
  StateT s k s
getT = StateT $ \s -> pure (s, s)

-- | A `StateT` where the resulting state is seeded with the given value.
--
-- >>> runStateT (putT 2) 0
-- ((),2)
--
-- >>> runStateT (putT 2 :: StateT Int List ()) 0
-- [((),2)]
putT ::
  Applicative k =>
  s
  -> StateT s k ()
putT s = StateT $ const $ pure ((), s)

-- | Remove all duplicate elements in a `List`.
--
-- /Tip:/ Use `filtering` and `State'` with a @Data.Set#Set@.
--
-- prop> \xs -> distinct' xs == distinct' (flatMap (\x -> x :. x :. Nil) xs)
distinct' ::
  Ord a =>
  List a
  -> List a
distinct' xs = eval' (filtering f xs) S.empty
  where
    f x = getT >>= \s -> putT (S.insert x s) >> pure (S.notMember x s)

-- | Remove all duplicate elements in a `List`.
-- However, if you see a value greater than `100` in the list,
-- abort the computation by producing `Empty`.
--
-- /Tip:/ Use `filtering` and `StateT` over `Optional` with a @Data.Set#Set@.
--
-- >>> distinctF $ listh [1,2,3,2,1]
-- Full [1,2,3]
--
-- >>> distinctF $ listh [1,2,3,2,1,101]
-- Empty
distinctF ::
  (Ord a, Num a) =>
  List a
  -> Optional (List a)
distinctF xs = evalT (filtering f xs) S.empty
  where
    f x
      | x > 100   = StateT $ const Empty
      | otherwise = getT >>= \s -> putT (S.insert x s) >> pure (S.notMember x s)

-- | An `OptionalT` is a functor of an `Optional` value.
data OptionalT k a =
  OptionalT {
    runOptionalT ::
      k (Optional a)
  }

-- | Implement the `Functor` instance for `OptionalT k` given a Functor k.
--
-- >>> runOptionalT $ (+1) <$> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty]
instance Functor k => Functor (OptionalT k) where
  (<$>) ::
    (a -> b)
    -> OptionalT k a
    -> OptionalT k b
  f <$> oa = OptionalT $ (f <$>) <$> (runOptionalT oa)

-- | Implement the `Applicative` instance for `OptionalT k` given a Monad k.
--
-- /Tip:/ Use `onFull` to help implement (<*>).
--
-- >>> runOptionalT $ OptionalT Nil <*> OptionalT (Full 1 :. Full 2 :. Nil)
-- []
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Full (+2) :. Nil) <*> OptionalT Nil
-- []
--
-- >>> runOptionalT $ OptionalT (Empty :. Nil) <*> OptionalT (Empty :. Nil)
-- [Empty]
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Empty :. Nil) <*> OptionalT (Empty :. Nil)
-- [Empty,Empty]
--
-- >>> runOptionalT $ OptionalT (Empty :. Nil) <*> OptionalT (Full 1 :. Full 2 :. Nil)
-- [Empty]
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Empty :. Nil) <*> OptionalT (Full 1 :. Full 2 :. Nil)
-- [Full 2,Full 3,Empty]
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Full (+2) :. Nil) <*> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty,Full 3,Empty]
instance Monad k => Applicative (OptionalT k) where
  pure ::
    a
    -> OptionalT k a
  pure = OptionalT . pure . pure

  (<*>) ::
    OptionalT k (a -> b)
    -> OptionalT k a
    -> OptionalT k b
  oab <*> oa = do
    ab <- oab
    a <- oa
    pure $ ab a

-- | Implement the `Monad` instance for `OptionalT k` given a Monad k.
--
-- >>> runOptionalT $ (\a -> OptionalT (Full (a+1) :. Full (a+2) :. Nil)) =<< OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Full 3,Empty]
instance Monad k => Monad (OptionalT k) where
  (=<<) ::
    (a -> OptionalT k b)
    -> OptionalT k a
    -> OptionalT k b
  f =<< oa = OptionalT $
    runOptionalT oa >>= \case
      Full a -> runOptionalT $ f a
      Empty -> pure Empty

-- | A `Logger` is a pair of a list of log values (`[l]`) and an arbitrary value (`a`).
data Logger l a =
  Logger (List l) a
  deriving (Eq, Show)

-- | Implement the `Functor` instance for `Logger
--
-- >>> (+3) <$> Logger (listh [1,2]) 3
-- Logger [1,2] 6
instance Functor (Logger l) where
  (<$>) ::
    (a -> b)
    -> Logger l a
    -> Logger l b
  f <$> Logger xs a = Logger xs (f a)

-- | Implement the `Applicative` instance for `Logger`.
--
-- >>> pure "table" :: Logger Int P.String
-- Logger [] "table"
--
-- >>> Logger (listh [1,2]) (+7) <*> Logger (listh [3,4]) 3
-- Logger [1,2,3,4] 10
instance Applicative (Logger l) where
  pure ::
    a
    -> Logger l a
  pure = Logger Nil

  (<*>) ::
    Logger l (a -> b)
    -> Logger l a
    -> Logger l b
  Logger xs ab <*> Logger ys a = Logger (xs ++ ys) (ab a)

-- | Implement the `Monad` instance for `Logger`.
-- The `bind` implementation must append log values to maintain associativity.
--
-- >>> (\a -> Logger (listh [4,5]) (a+3)) =<< Logger (listh [1,2]) 3
-- Logger [1,2,4,5] 6
instance Monad (Logger l) where
  (=<<) ::
    (a -> Logger l b)
    -> Logger l a
    -> Logger l b
  f =<< Logger xs a = let Logger ys b = f a
                      in Logger (xs ++ ys) b

-- | A utility function for producing a `Logger` with one log value.
--
-- >>> log1 1 2
-- Logger [1] 2
log1 ::
  l
  -> a
  -> Logger l a
log1 x = Logger (x:.Nil)

-- | Remove all duplicate integers from a list. Produce a log as you go.
-- If there is an element above 100, then abort the entire computation and produce no result.
-- However, always keep a log. If you abort the computation, produce a log with the value,
-- "aborting > 100: " followed by the value that caused it.
-- If you see an even number, produce a log message, "even number: " followed by the even number.
-- Other numbers produce no log message.
--
-- /Tip:/ Use `filtering` and `StateT` over (`OptionalT` over `Logger` with a @Data.Set#Set@).
--
-- >>> distinctG $ listh [1,2,3,2,6]
-- Logger ["even number: 2","even number: 2","even number: 6"] (Full [1,2,3,6])
--
-- >>> distinctG $ listh [1,2,3,2,6,106]
-- Logger ["even number: 2","even number: 2","even number: 6","aborting > 100: 106"] Empty
distinctG ::
  forall a.
  (Integral a, Show a) =>
  List a
  -> Logger Chars (Optional (List a))
-- distinctG xs = runOptionalT (evalT (filtering (\a -> StateT (\s ->
--   OptionalT (f a s))) xs) S.empty)
--   where
--     f :: a -> S.Set a -> Logger Chars (Optional (Bool, S.Set a))
--     f a s
--       | a > 100   = log1 (fromString ("aborting > 100: " P.++ show a)) Empty
--       | even a    = log1 (fromString ("even number: " P.++ show a)) set
--       | otherwise = pure set
--       where
--         set = Full (S.notMember a s, S.insert a s)
distinctG xs = runOptionalT (evalT (filtering (\a -> StateT (\s ->
  OptionalT (mkPred logger s a))) xs) S.empty)
  where
    mkPred l s a = case l a of
      Left f -> f Empty
      Right f -> f $ Full (S.notMember a s, S.insert a s)

    logger a
      | a > 100   = Left $ log1 (fromString ("aborting > 100: " P.++ show a))
      | even a    = Right $ log1 (fromString ("even number: " P.++ show a))
      | otherwise = Right $ pure

onFull ::
  Applicative k =>
  (t -> k (Optional a))
  -> Optional t
  -> k (Optional a)
onFull g o =
  case o of
    Empty ->
      pure Empty
    Full a ->
      g a
