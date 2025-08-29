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
            [ memberBench 10000 cf10k [5000..15000]
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

memberBench :: Int -> CuckooFilter (PrimState IO) 4 10 Int -> [Int] -> Benchmark
memberBench n cf xs = bench (show n) $ whnfIO f
  where
    f = fmap length (filterM (member cf) xs)
