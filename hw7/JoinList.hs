module JoinList where

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)


tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m a) = m
tag (Append m x y) = m


(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty Empty = Empty
(+++) Empty l2 = l2
(+++) l1 Empty = l1
(+++) l1 l2 = Append (tag l1 <> tag l2) l1 l2
