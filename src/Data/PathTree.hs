module Data.PathTree where

import qualified Data.Map as M

data PathTree k v = PathTree (Maybe v) (M.Map k (PathTree k v))
  deriving (Eq, Show)

empty :: PathTree k v'
empty = PathTree Nothing M.empty

find :: Ord k => PathTree k v -> [k] -> Maybe v
find (PathTree v mp) []     = v
find (PathTree v mp) (k:ks) = mp M.!? k >>= \t -> find t ks

insert :: Ord k => [k] -> v -> PathTree k v -> PathTree k v
insert [] v (PathTree _ mp) = PathTree (Just v) mp
insert (k:ks) v (PathTree v' mp) =
  PathTree v' $ M.insert k (insert ks v empty) mp
