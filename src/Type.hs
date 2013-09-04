{-# LANGUAGE DeriveDataTypeable #-}

module Type where

import Data.Data
import Data.Typeable

data XMLAST =  
    Element Name [Attribute] [XMLAST]
  | Body String
  | Comment String
  | Schema String
  | CouldNotParse String
  deriving (Show, Data, Typeable)

data Attribute = Attribute String AttrValue
  deriving(Show,Data,Typeable)

data AttrValue = UrCode String
               | QuotedString Char String
               | Number String
  deriving(Show,Data,Typeable)


type Name      = String
type Key       = String
type Value     = String 

isSchema (Schema _) = True
isSchema _ = False
