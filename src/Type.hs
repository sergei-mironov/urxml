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

newtype Attribute = Attribute String
  deriving(Show,Data,Typeable)

type Name      = String
type Key       = String
type Value     = String 

isSchema (Schema _) = True
isSchema _ = False
