module Main where

import Criterion.Main
import StrictList (StrictList, fromList, map, mapStack)
import Prelude hiding (map)

bySize :: Int -> Benchmark
bySize size =
  let !inputList = fromList [1 .. size]
   in bgroup
        (show size)
        [ bench "map" $ nf (map (+ 1)) inputList,
          bench "mapStack" $ nf (mapStack (+ 1)) inputList
        ]

main :: IO ()
main =
  defaultMain
    [ bySize 10_000,
      bySize 100_000,
      bySize 1_000_000,
      bySize 10_000_000
    ]
