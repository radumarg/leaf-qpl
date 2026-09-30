module Frontend.Helper

import Data.SortedMap
import Data.List

%default total

public export
maxKey : Ord k => SortedMap k v -> Maybe k
maxKey m =
  case keys m of
    [] => Nothing
    k :: ks => Just (foldl max k ks)

public export
maxValue : Ord v => SortedMap k v -> Maybe v
maxValue m =
  case values m of
    [] => Nothing
    v :: vs => Just (foldl max v vs)

public export
keyWithMaxValue : Ord v => SortedMap k v -> Maybe k
keyWithMaxValue m =
  case Data.SortedMap.toList m of
    [] => Nothing
    entry :: rest => Just (fst (foldl choose entry rest))
  where
    choose : (k, v) -> (k, v) -> (k, v)
    choose best candidate =
      if snd candidate > snd best
         then candidate
         else best

