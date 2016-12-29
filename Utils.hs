{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
module Utils where

import           Control.Lens
import qualified Data.Set              as S
import           Data.Maybe
import           Data.Tuple.Extra
import           Prelude.Unicode
import           System.Random.Shuffle as Sys
import           System.IO.Unsafe      as Sys


-- * Generic utils

remove_duplicates ∷ Ord a ⇒ [a] → [a]
remove_duplicates = S.toList . S.fromList

shuffle_list ∷ [a] → [a]
shuffle_list = Sys.unsafePerformIO ∘ Sys.shuffleM

move_list_head ∷ Int → [a] → [a] → ([a], [a])
move_list_head n from to
  = let new_from = drop n from
        new_to   = take n from ++ to
    in (from, to)

split_by ∷ Int → (a → Bool) → [a] → ([a], [a])
split_by n f xs = _split_by n f xs ([], [])
  where _split_by 0 f xs     (acct, accf) = (acct, accf ++ xs)
        _split_by n f []     (acct, accf) = (acct, accf)
        _split_by n f (x:xs) (acct, accf) = _split_by (n - 1) f xs
                                            $ if f x
                                              then (x:acct,   accf)
                                              else (  acct, x:accf)

mxtract ∷ Int → [a] → Maybe (a, [a])
mxtract = loop []
  where
    loop acc 0 = fmap (second (reverse acc ++)) ∘ uncons
    loop acc n = \(x:xs) → loop (x:acc) (n - 1) xs
