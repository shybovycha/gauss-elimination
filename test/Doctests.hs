module Main where

import Test.DocTest (doctest)

main = doctest
  [ "-isrc"
  , "src/Fraction.hs"
  , "src/Map.hs"
  , "src/Set.hs"
  , "src/Parsing.hs"
  , "src/EquationParser.hs"
  , "src/Gauss.hs"
  ]
