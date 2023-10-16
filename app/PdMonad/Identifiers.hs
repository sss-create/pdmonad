{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module PdMonad.Identifiers where

import PdMonad.Core
import Data.Maybe (fromMaybe)
import Data.List (nubBy)
import Data.Function (on)
import Data.Text qualified as T
import Data.Text.IO qualified as T


defPdObject :: PdObject
defPdObject = PdObject Nothing "#X obj" (50, 75) ""


obj :: T.Text -> PdObject
obj args = defPdObject {objectArguments = escapeSpecial args}


mult :: Float -> PdObject
mult args = defPdObject {objectArguments = T.concat ["* ", T.pack $ show args]}


msg :: T.Text -> PdObject
msg args = defPdObject {objectType = "#X msg", objectArguments = escapeSpecial args}


pdprint :: PdObject
pdprint = defPdObject {objectArguments = "print"}


loadbang :: PdObject
loadbang = defPdObject {objectArguments = "loadbang"}


bang :: PdObject
bang =
  let args = "bng 19 250 50 0 empty empty empty 0 -10 0 12 #fcfcfc #000000 #000000;"
  in defPdObject {objectArguments = args}


toggle :: PdObject
toggle =
  let args = "tgl 19 0 empty empty empty 0 -10 0 12 #fcfcfc #000000 #000000 0 1;"
  in defPdObject {objectArguments = args}


number :: PdObject
number =
  let args = "5 0 0 0 - - - 0;"
  in defPdObject {objectType = "#X floatatom", objectArguments = args}
