# pdmonad
## Write Pure Data patches in Haskell.
PdMonad is an eDSL/library/program for writing Pure Data patches through text. 

It uses [graphviz](https://hackage.haskell.org/package/graphviz) for auto layouting the generated patch.

current patch creation:
```haskell
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
```

use with:
```
cabal run
```

"compiles" to:

![Pd created patch](resources/img/layouted.png)



## Examples for creating objects/messages/numbers:

```obj "pack f f f f"``` for creating objects.

```msg "hello, world``` for messages.

```num``` for a number.

always use ```#``` to specify the object/message/number id, which is relevant for the layouting.

e.g.:
```obj "osc~ 220" # 3```

```-->``` creates a connection between two objects (currently connecting the first inlet of the leftside object with the first outlet of the next object).


# Structure of the generated patch

A pd patch consists of a list of objects with coordinate infos and arguments + a list of connections. the objects id's are specified by order of appeareance:
```
#N canvas 0 50 450 300 12;
#X floatatom 216.0 150.0 5 0 0 0 - - - 0;;
#X msg 216.0 194.0 \$1;
#X obj 216.0 238.0 print;
#X obj 90.0 62.0 loadbang;
#X msg 27.0 106.0 hello\, world;
#X obj 27.0 150.0 print;
#X msg 153.0 106.0 3;
#X obj 279.0 18.0 osc~ 220;
#X obj 279.0 62.0 *~ 1;
#X obj 279.0 106.0 cyclone/snapshot~ 50;
#X connect 0 0 1 0;
#X connect 1 0 2 0;
#X connect 3 0 4 0;
#X connect 4 0 5 0;
#X connect 3 0 6 0;
#X connect 6 0 0 0;
#X connect 7 0 8 0;
#X connect 8 0 9 0;
#X connect 9 0 0 0;
```