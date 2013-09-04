module Printer where

import Control.Monad.Writer
import Text.ParserCombinators.Parsec
import Text.Printf

import Type

wordwrap tw s = reverse $ map unwords $ foldl filln [[]] (words s) where
  filln (ws:lines) w = if (length (unwords (ws++[w]))) > tw then [w]:ws:lines else (ws++[w]):lines

data Doc = Doc String [Doc] String
         | DocL String
         | DocT String
         deriving(Show)

raw :: Doc -> String
raw (Doc o ds c) = o ++ (concat $ map raw ds) ++ c
raw (DocL s) = unwords (words s)
raw (DocT s) = unwords (words s)

rawlen = length . raw

data PPConfig = PPConfig {
    textWidth :: Int
  , rightMargin :: Int
  , startIndent :: Int
  , tabStop :: Int
  , expandTab :: Bool
  , skip1stLineIndent :: Bool
} deriving(Show)

defaultConfig = PPConfig 80 20 0 4 False False

ppDoc :: PPConfig -> Bool -> Doc -> String
ppDoc c b d = execWriter (ppDoc' c b d) where
  ppDoc' _ True doc = tell $ raw doc
  ppDoc' cfg' False doc = do
    case rawlen doc > 0 of
      False -> return ()
      True -> case (rawlen doc) < l1 of
        True -> tell indent1 >> ppDoc' cfg True doc >> tell "\n"
        False -> case doc of
          (Doc o ds c) -> do
            line (indent1 ++ o)
            mapM_ (ppDoc' cfg{startIndent = i + ts} False) ds
            line (indent ++ c)
          (DocL s) -> line (indent1 ++ s)
          (DocT s) -> forM_ (wordwrap l1 s`zip`[1..]) $ \(l,n) -> 
                        case n == 1 of
                          True -> line (indent1 ++ l)
                          False -> line (indent ++ l)
    where
      cfg = cfg' {skip1stLineIndent = False}
      ts = tabStop cfg
      tw = textWidth cfg
      margin = rightMargin cfg
      i = startIndent cfg
      indent1 = if skip1stLineIndent cfg' then [] else indent
      indent = if expandTab cfg then replicate i ' ' else (replicate (i`div`ts) '\t') ++ (replicate (i`mod`ts) ' ')
      -- l1 = if i >= (tw-margin) then tw else tw - i
      l1 = tw
      line s = tell (s ++ "\n")

xml2doc :: XMLAST -> Doc
xml2doc ast = 
  case ast of
    (Element name attrs []) -> DocL $ printf "<%s%s/>" name (printAttrs attrs)
    (Element name attrs asts) -> let
      open = printf "<%s%s>" name (printAttrs attrs)
      close = printf "</%s>" name
      in Doc open (map xml2doc asts) close
    (Body s) -> DocT s
    -- FIXME: Minor bug in the Ur/Web : multiline comments breaks line counter
    -- We print comments as single lines
    -- (Comment s) -> Doc "<!--" [DocT s] "-->"
    (Comment s) -> DocL $ "<!--" ++ s ++ "-->"
    (Schema s) -> DocL $ "<!" ++ s ++  ">"
    s -> error $ "Could not print " ++ (show s)
  where
    printAttrs [] = ""
    printAttrs ((Attribute a v):kvs) = (printf " %s=%s" a (printValue v)) ++ (printAttrs kvs)
    printValue (UrCode ur) = printf "{%s}" ur
    printValue (QuotedString q s) = printf "%c%s%c" q s q
    printValue (Number s) = s

prettyPrint :: PPConfig -> XMLAST -> String
prettyPrint cfg = ppDoc cfg False . xml2doc

printLine :: XMLAST -> String
printLine = ppDoc defaultConfig True . xml2doc


