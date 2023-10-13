# pdmonad
## Write Pure Data patches inside Haskell.

use with:
```
cabal run
```

current patch creation:
```haskell
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
```

becomes:

![Pd created patch](resources/img/created.png)
## Non-Native Identifier
Use e.g. ```obj "pack f f f f"``` for creating objects without a (currently) native identifier. Remember to assign the ```objectId```.


# General file structure of a patch
![Pd base patch](resources/img/base.png)

e.g.:
```
#N canvas 940 104 502 581 12;
#X obj 72 67 loadbang;
#X msg 72 91 220;
#X floatatom 72 115 5 0 0 0 - - - 0;
#X obj 72 138 osc~;
#X obj 72 162 *~ 1;
#X obj 72 186 dac~;
#X connect 0 0 1 0;
#X connect 1 0 2 0;
#X connect 2 0 3 0;
#X connect 3 0 4 0;
#X connect 4 0 5 0;
#X connect 4 0 5 1;
```

# Developed/Tested with:
```
GHC 9.4.5
Cabal 3.6.2.0
Pure Data 0.53.1
```
