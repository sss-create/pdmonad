{-# LANGUAGE OverloadedStrings #-}

module Main where

import PdMonad.Core
import PdMonad.Identifiers as Pd

num = Pd.number # 0
lb = Pd.loadbang # 3
dollarOne = Pd.msg "$1" # 1
hello = Pd.msg "hello, world"
loadedMsg = Pd.msg "999"


main :: IO ()
main =  writePatch "mypatch.pd"
    [
        newCol --> num --> dollarOne --> pdprint #2,
        newCol --> lb --> hello #4 --> pdprint #5,
        newCol --> lb --> loadedMsg #6 --> num
    ]
