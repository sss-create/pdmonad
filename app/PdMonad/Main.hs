{-# LANGUAGE OverloadedStrings #-}

module Main where

import PdMonad.Core
import PdMonad.Identifiers

number = num # 0
lbang = obj "loadbang" # 3
variable = msg "$1" # 1
hworld = msg "hello, world"
three = msg "3"

start = PdPatch [] []

main :: IO ()
main =  writePatch "mypatch.pd" $ preparePatch
    [ 
      start --> number --> variable --> obj "print" # 2,
      start --> lbang --> hworld # 4 --> obj "print" # 5,
      start --> lbang --> three # 6 --> number,
      start --> obj "osc~ 220" # 7 --> obj "*~ 1" # 8 --> obj "cyclone/snapshot~ 50" # 9 --> num
    ]
