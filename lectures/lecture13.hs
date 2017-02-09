import Control.Monad
import Data.Maybe

data SM s a = SM (s -> (s,a))

instance Functor (SM s) where
   fmap f (SM a) = SM $ \s -> let (s', a') = a s in (s', f a')

instance Applicative (SM s) where
   pure a = SM (\s -> (s,a))
   SM f <*> (SM a) = SM $ \s ->
     let (s',f') = f s
         (s'', a') = a s'
     in (s'', f' a')

instance Monad (SM s) where

   return a = SM (\s -> (s,a))

   SM sm0 >>= fsm1 = SM $ \s0 ->
     let (s1,a1) = sm0 s0  -- left computation on the state
         SM sm1 = fsm1 a1  -- the computation of the "right monad"
         (s2,a2) = sm1 s1  -- right computation on the state
     in (s2,a2)

runSM :: SM s a -> s -> a
runSM (SM sm0) = snd . sm0

set :: s -> SM s ()
set a = SM (\_ -> (a, ()))

get :: SM s s
get = SM (\s -> (s, s))

-- 1.1
nop :: SM s ()
nop = SM (\s -> (s, ()))

reset :: SM Int ()
reset = SM (\_ -> (0, ()))

update :: (s -> s) -> SM s ()
update f = SM (\s -> (f s ,()))

nop' :: SM s ()
nop' = get >>= set

reset' :: SM Int ()
reset' = set 0

update' :: (s -> s) -> SM s ()
update' f = f >>= set
