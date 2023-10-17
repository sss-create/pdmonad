{-# LANGUAGE OverloadedStrings #-}

module Main where

import PdMonad.Core
import PdMonad.Identifiers

num = number # 0
lb = obj "loadbang" # 3
dollarOne = msg "$1" # 1
hello = msg "hello, world"
loadedMsg = msg "999"


main :: IO ()
main =  writePatch "mypatch.pd"
    [
        PdPatch [] [] --> num --> dollarOne --> obj "print" # 2,
        PdPatch [] [] --> lb --> hello # 4 --> obj "print" # 5,
        PdPatch [] [] --> lb --> loadedMsg # 6 --> num,
        PdPatch [] [] --> 
            obj "osc~ 220" # 7 --> obj "*~ 1" # 8 --> obj "cyclone/snapshot~ 50" # 9 --> num
    ]
