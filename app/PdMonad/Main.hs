{-# LANGUAGE OverloadedStrings #-}

module Main where

import PdMonad.Core
import PdMonad.Identifiers

number = num # 0
lbang = obj "loadbang" # 3
variable = msg "$1" # 1
hworld = msg "hello, world"
three = msg "3"


main :: IO ()
main =  writePatch "mypatch.pd"
    [
        PdPatch [] [] --> number --> variable --> obj "print" # 2,
        PdPatch [] [] --> lbang --> hworld # 4 --> obj "print" # 5,
        PdPatch [] [] --> lbang --> three # 6 --> number,
        PdPatch [] [] --> 
            obj "osc~ 220" # 7 --> obj "*~ 1" # 8 --> obj "cyclone/snapshot~ 50" # 9 --> num
    ]
