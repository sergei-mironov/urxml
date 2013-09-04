module Main where

import Control.Monad
import System.Environment
import System.Process
import System.Exit
import System.IO
import System.FilePath
import Text.Printf
import Data.Either
import Data.Generics
import Data.Char
import Data.List
import Data.Data
import Data.Typeable
import Data.Maybe
import Options.Applicative

import Type
import Printer
import Parser

data Args = A
  { ppopts :: PPConfig
  , noSchema :: Bool
  -- , pipe :: String
  , ur :: Bool
  , files :: [FilePath]
  }

pargs :: Parser Args
pargs = A
  <$> (PPConfig
    <$> option
        (  long "text-width"
        <> short 'w'
        <> metavar "INT"
        <> value 80
        <> help "Recommended text width (not strict)" )
    <*> option
        (  long "right-margin"
        <> short 'm'
        <> metavar "INT"
        <> value 10
        <> help "Right margin" )
    <*> option
        (  long "start-indent"
        <> short 'i'
        <> metavar "INT"
        <> value 0
        <> help "Indent to start from" )
    <*> option
        (  long "tab-stop"
        <> short 's'
        <> metavar "INT"
        <> value 4
        <> help "Tab stop" )
    <*> flag True False
        (  long "expand-tab"
        <> short 'e'
        <> help "Expand tab" )
    <*> flag False True
        (  long "skip-first"
        <> short '1'
        <> help "Don't indent 1st line" )
    )
  <*> flag False True
        (  long "skip-schema"
        <> short 'S'
        <> help "Don't print DOCTYPE and stuff" )
  -- <*> strOption
  --       (  long "pipe-attr"
  --       <> value []
  --       <> help "Run sed for each link,href,url,etc" )
  <*> flag False True
        (  long "ur-attrs"
        <> short 'u'
        <> help "Convert tag attributes to the Ur/Web format" )
  <*> arguments str ( metavar "FILE" <> help "File or `stdin' to read from the stdin" )

main :: IO ()
main = execParser opts >>= main_
  where
    opts = info (helper <*> pargs)
      (  fullDesc
      <> progDesc "Ur/Web XML indenter (Ur/Web dialect of XML supports {}-style attributes)."
      <> header "XML converter/indenter" )

readInput "stdin" = hGetContents stdin
readInput f = readFile f

main_ (A cfg _ _ []) = fail "No input files"
main_ (A cfg ss ur inf) = do
  forM_ inf $ \f -> do
    (readInput f >>= return . parseXML') >>=
      return . (if ss then filter (not . isSchema) else id) >>=
          mapM_ ( -- (if (not (null pl)) then pipeAttrs pl else return . id) >=>
                  (if (ur) then return . urAttrs else return . id) >=>
                  putStrLn . prettyPrint cfg
                )

-- pipeAttrs :: String -> XMLAST -> IO XMLAST
-- pipeAttrs cmd ast = everywhereM (mkM inattr) ast where
--   inattr (Attribute a) = do
--     (i,o,e,_) <- runInteractiveCommand cmd
--     hPutStr i (printAttribute a)
--     hClose i
--     a' <- hGetContents o
--     hClose e
--     return $ Attribute a'
        
urAttrs :: XMLAST -> XMLAST
urAttrs ast = everywhere (mkT inattr) ast where
  inattr (Attribute a' v@(UrCode _)) = Attribute a' v
  inattr (Attribute a' v@(QuotedString c s))
    | a == "src" = Attribute a' (UrCode $ printf "static Static.%s" (static s))
    | a == "href" = Attribute "link" (UrCode $ printf "%s st" (takeBaseName s))
    | a == "class" = Attribute a' (UrCode $ s)
    | a `elem` ["width","height"] = Attribute a' (Number s)
    | otherwise = Attribute a' v
    where
      a = map toLower a'
      static f = upper $ map nodots $ takeFileName f
      nodots x | x`elem`".-" = '_' | otherwise = x
      upper x | null x = [] | otherwise = (toUpper (head x) : tail x)
  inattr (Attribute a v@(Number _)) = Attribute a v

