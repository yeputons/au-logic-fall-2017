module InterleavingStream(InterleavingStream, elements) where
import Control.Applicative
import Control.Monad.Plus
import Debug.Trace

newtype InterleavingStream a = IS { elements :: [a] }
  
instance Functor InterleavingStream where
  fmap f (IS l) = IS $ fmap f l

interleaveTwo :: [a] -> [a] -> [a]
interleaveTwo []     ys = ys
interleaveTwo (x:xs) ys = x:interleaveTwo ys xs

interleaveAll :: [[a]] -> [a]
interleaveAll = foldr interleaveTwo []

instance Applicative InterleavingStream where
  pure a = IS [a]
  (IS fs) <*> (IS xs) = IS $ interleaveAll (map (\f -> map f xs) fs)

instance Monad InterleavingStream where
  (IS xs) >>= f = IS $ interleaveAll (map (elements . f) xs)

instance Alternative InterleavingStream where
  empty = IS []
  (IS xs) <|> (IS ys) = IS $ interleaveTwo xs ys

instance MonadPlus InterleavingStream where
