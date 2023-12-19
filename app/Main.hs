{-# LANGUAGE OverloadedStrings #-}

module Main where

import PdMonad

number = num # 0
lbang = obj "loadbang" # 3
variable = msg "$1" # 1
hworld = msg "hello, world"
three = msg "3"


main :: IO ()
main = 
  writePatch "mypatch.pd" $ preparePatch
  [ 
    number --> variable --> (obj "print" # 2),
    lbang --> (hworld # 4) --> (obj "print" # 5),
    lbang --> (three # 6) --> number,
    (obj "osc~ 220" # 7) --> (obj "*~ 1" # 8) --> (obj "cyclone/snapshot~ 50" # 9) --> num
  ]
