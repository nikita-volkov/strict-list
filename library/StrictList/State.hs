module StrictList.State
where

import StrictList (List)
import StrictList.Prelude
import qualified StrictList as Defs


prepend :: MonadState (List a) m => List a -> m ()
prepend list = modify (list <>)

prependReversed :: MonadState (List a) m => List a -> m ()
prependReversed list = modify (Defs.prependReversed list)

append :: MonadState (List a) m => List a -> m ()
append list = modify (<> list)

map :: MonadState (List a) m => (a -> a) -> m ()
map = modify . fmap

mapReversed :: MonadState (List a) m => (a -> a) -> m ()
mapReversed = modify . Defs.mapReversed

reverse :: MonadState (List a) m => m ()
reverse = modify Defs.reverse

take :: MonadState (List a) m => Int -> m ()
take = modify . Defs.take

takeReversed :: MonadState (List a) m => Int -> m ()
takeReversed = modify . Defs.takeReversed

drop :: MonadState (List a) m => Int -> m ()
drop = modify . Defs.drop

filter :: MonadState (List a) m => (a -> Bool) -> m ()
filter = modify . Defs.filter

filterReversed :: MonadState (List a) m => (a -> Bool) -> m ()
filterReversed = modify . Defs.filterReversed

takeWhile :: MonadState (List a) m => (a -> Bool) -> m ()
takeWhile = modify . Defs.takeWhile

takeWhileReversed :: MonadState (List a) m => (a -> Bool) -> m ()
takeWhileReversed = modify . Defs.takeWhileReversed

dropWhile :: MonadState (List a) m => (a -> Bool) -> m ()
dropWhile = modify . Defs.dropWhile

takeWhileFromEnding :: MonadState (List a) m => (a -> Bool) -> m ()
takeWhileFromEnding = modify . Defs.dropWhileFromEnding

dropWhileFromEnding :: MonadState (List a) m => (a -> Bool) -> m ()
dropWhileFromEnding = modify . Defs.dropWhileFromEnding

uncons :: MonadState (List a) m => m (Maybe a)
uncons = state $ \ case
  Defs.Cons head tail -> (Just head, tail)
  _ -> (Nothing, Defs.Nil)

head :: MonadState (List a) m => m (Maybe a)
head = gets Defs.head

last :: MonadState (List a) m => m (Maybe a)
last = gets Defs.last

tail :: MonadState (List a) m => m ()
tail = modify Defs.tail

init :: MonadState (List a) m => m ()
init = modify Defs.init

initReversed :: MonadState (List a) m => m ()
initReversed = modify Defs.initReversed

apZipping :: MonadState (List a) m => List (a -> a) -> m ()
apZipping = modify . Defs.apZipping

apZippingReversed :: MonadState (List a) m => List (a -> a) -> m ()
apZippingReversed = modify . Defs.apZippingReversed

apReversed :: MonadState (List a) m => List (a -> a) -> m ()
apReversed = modify . Defs.apReversed

explodeReversed :: MonadState (List a) m => (a -> List a) -> m ()
explodeReversed = modify . Defs.explodeReversed
