module Main where

import Criterion.Main
import StrictList (StrictList, fromList, map, mapStack)
import Prelude hiding (map)

size :: Int
size = 5000000

inputList :: StrictList Int
inputList = fromList [1 .. size]

main :: IO ()
main =
  defaultMain
    [ bench "map (+1)" $ nf (map (+ 1)) inputList,
      bench "mapStack (+1)" $ nf (mapStack (+ 1)) inputList
    ]
