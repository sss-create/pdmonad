{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module PdMonad.Pd where

import Data.Text qualified as T


data PdObject = PdObject
  { objectId          :: Maybe Int
  , objectType        :: T.Text
  , objectCoordinates :: (Double, Double)
  , objectArguments   :: T.Text
  } deriving (Show, Eq)


data PdConnection = PdConn
  { source :: Maybe Int
  , outlet :: Int
  , sink   :: Maybe Int
  , inlet  :: Int
  } deriving (Show)


data PdPatch = PdPatch
  { pdObjects     :: [PdObject]
  , pdConnections :: [PdConnection]
  } deriving (Show)


defPdObject :: PdObject
defPdObject = PdObject Nothing "#X obj" (50, 75) ""


obj :: T.Text -> PdObject
obj args = defPdObject {objectArguments = escapeSpecial args}


msg :: T.Text -> PdObject
msg args = defPdObject {objectType = "#X msg", objectArguments = escapeSpecial args}


bang :: PdObject
bang =
  let args = "bng 19 250 50 0 empty empty empty 0 -10 0 12 #fcfcfc #000000 #000000;"
  in defPdObject {objectArguments = args}


toggle :: PdObject
toggle =
  let args = "tgl 19 0 empty empty empty 0 -10 0 12 #fcfcfc #000000 #000000 0 1;"
  in defPdObject {objectArguments = args}


num :: PdObject
num =
  let args = "5 0 0 0 - - - 0;"
  in defPdObject {objectType = "#X floatatom", objectArguments = args}


escapeSpecial :: T.Text -> T.Text
escapeSpecial n =
  let specials = [",", "$"]
  in foldl (\t s -> T.replace s (T.append "\\" s) t) n specials
