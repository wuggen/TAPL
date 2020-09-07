module Tapl.Util
  ( unwrapMaybe
  , unwrapLeft
  , unwrapRight
  , fmapLeft
  , tryEither
  , (!!?)
  ) where

unwrapMaybe :: Maybe a -> a
unwrapMaybe = maybe undefined id

unwrapLeft :: Either e a -> e
unwrapLeft = either id undefined

unwrapRight :: Either e a -> a
unwrapRight = either undefined id

fmapLeft :: (e -> f) -> Either e a -> Either f a
fmapLeft f (Left e) = Left (f e)
fmapLeft _ (Right a) = Right a

tryEither :: Either a b -> Either a b -> Either a b
tryEither a b = either (const b) (const a) a

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
(a:_) !!? 0 = Just a
(_:as) !!? n = as !!? (n - 1)
