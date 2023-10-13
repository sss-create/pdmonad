{-# LANGUAGE OverloadedStrings #-}

module Main where
import Pdmonad

num = number #0
lb = loadbang #3
dollarOne = msg "\\$1" #1
hello = msg "hello \\, world"
loadedMsg = msg "999"


main :: IO ()
main = writePatch "mypatch.pd"
    [
        column --> num --> dollarOne --> pdprint #2,
        column --> lb --> hello #4 --> pdprint #5,
        column --> lb --> loadedMsg #6 --> num
    ]
