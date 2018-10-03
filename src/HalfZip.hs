module HalfZip where

class Functor f => HalfZip f where
  halfZip :: f x -> f y -> Maybe (f (x, y))

{-
halfZip         fx          fy  = fmap (fmap swap) (halfZip fy fx)
halfZip (fmap g fx) (fmap h fx) = Just (fmap (f &&& g) fx)
halfZip fx fy = Just fxy  ->  fmap fst fxy = fx  *  fmap snd fxy = fy
halfZip fx fy = Nothing   ->  fmap (const ()) fx /= fmap (const ()) fy

-- are the above redundant?
-}