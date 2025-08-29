{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: Main
-- Copyright: Copyright © 2019-2021 Lars Kuhtz <lakuhtz@gmail.com>
-- License: BSD3
-- Maintainer: Lars Kuhtz <lakuhtz@gmail.com>
-- Stability: experimental
--
module Main
( main
) where

import Control.Monad (filterM)
import Control.Monad.Primitive (PrimState)
import Control.Monad.ST (runST)
import Criterion
import Criterion.Main
import Data.Foldable (traverse_)

import Data.Cuckoo

-- -------------------------------------------------------------------------- --
-- Main

main :: IO ()
main = do
    cf10k <- do
        cf <- newCuckooFilter @4 @10 @Int 0 12000
        traverse_ (insert cf) [1..10000]
        pure $! cf

    defaultMain
        [ bgroup "insert"
            [ insertBench 10000 12000 [1..10000]
            ]
        , bgroup "member"
            [ memberBench "1:1 hit:hiss" 10000 cf10k (interleave [5000..10000] [10001..15000])
            , memberBench "only hit" 10000 cf10k [1..10000]
            ]
        , bgroup "delete"
            [ deleteBench "all" 10000 cf10k [1..10000]
            ]
        ]

-- -------------------------------------------------------------------------- --

instance CuckooFilterHash Int

insertBench :: Int -> Int -> [Int] -> Benchmark
insertBench n cap xs0 = bench ("n-" <> show n <> "-cap-" <> show cap) $ whnf f xs0
  where
    f xs = runST $ do
        cf <- newCuckooFilter @4 @10 @Int 0 (fromIntegral cap)
        failed <- filterM (fmap not . insert cf) xs
        pure $! length failed

memberBench :: String -> Int -> CuckooFilter (PrimState IO) 4 10 Int -> [Int] -> Benchmark
memberBench label n cf xs = bench (label <> " - " <> show n) $ whnfIO f
  where
    f = fmap length (filterM (member cf) xs)

deleteBench :: String -> Int -> CuckooFilter (PrimState IO) 4 10 Int -> [Int] -> Benchmark
deleteBench label n cf0 xs = bench (label <> " - " <> show n) $ whnfIO f
  where
    f = do
        (cf, _) <- splitCuckooFilter cf0
        fmap length (filterM (delete cf) xs)



-- | Doesn't cause actual difference in bench.
interleave :: [a] -> [a] -> [a]
interleave (a:as) (b:bs) = a:b:interleave as bs
interleave as bs = as <> bs

