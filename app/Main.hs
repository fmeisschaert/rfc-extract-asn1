module Main where

import Control.Monad
import System.Environment
import Data.List
import Data.Char
import Text.Parsec
import Language.ASN1.Parser
import Text.Pretty.Simple
import System.Console.GetOpt

data Background = Light | Dark
  deriving (Eq,Show)

data Flag = ShowAST | NoText | Color Background
  deriving (Eq,Show)

type Flags = [Flag]

readBG :: String -> Flag
readBG ('d':_) = Color Dark
readBG ('D':_) = Color Dark
readBG _       = Color Light

options :: [ OptDescr Flag ]
options =
    [ Option ['a'] ["show-ast"] (NoArg ShowAST) "show abstract syntax tree"
    , Option ['n'] ["no-text"]  (NoArg NoText)  "don't show extracted text"
    , Option ['c'] ["color"]    (ReqArg readBG "Background")   "show tree in color with light or dark background"
    ]

showAST :: Flags -> Bool
showAST = any (== ShowAST)

noText :: Flags -> Bool
noText  = any (== NoText)

inColor :: Flags -> Maybe Background
inColor [] = Nothing
inColor (Color bg:_) = Just bg
inColor (_:flags) = inColor flags

printUsage :: String -> IO ()
printUsage progName = putStrLn $ usageInfo (title progName) options
  where
    title name = "usage: " ++ name ++ " {<option>} <rfc-text-file>\n"

main :: IO ()
main = do
    args <- getArgs
    let (flags,args',_) = getOpt RequireOrder options args
    case args' of
      []       -> getProgName >>= printUsage
      (file:_) -> processFile flags file

hashIndent :: String -> String
hashIndent s = '#' : ' ' : go s
  where
    go ('\n':rest) = '\n' : '#' : ' ' : go rest
    go (c:rest) = c : go rest
    go [] = []

printAssignText :: Show a => Flags -> (a,String) -> IO ()
printAssignText flags (asm,text) = do
    unless (noText  flags) $ tt text
    when   (showAST flags) $ pp asm
    putStrLn ""
  where
    pp = case inColor flags of
        Nothing -> pPrintNoColor
        Just Dark -> pPrint
        _         -> pPrintLightBg
    tt = putStrLn . hashIndent

processFile :: Flags -> FilePath -> IO ()
processFile flags file = do
    text <- readFile file
    let ls = rfcTextLines text
        as = productions ls
    mapM_ (printAssignText flags) as

rfcTextLines :: String -> [ String ]
rfcTextLines = go . lines
  where
    go (_:('\f':_):_:ls) = go ls
    go (_:('\f':_):[]) = []
    go (l:ls) = l : go ls
    go [] = []

nextProduction :: [ String ] -> String
nextProduction = fixup . dropWhile isSpace . unlines . nextProductionLine
  where
    nextProductionLine = dropWhile (not . isInfixOf "::=")

data LocatedAssignment = LA
    { laAssignment :: Assignment
    , laStart      :: SourcePos
    , laEnd        :: SourcePos
    , laRest       :: [String]
    }
  deriving (Show)

assignmentAndText :: Parsec String () (Assignment,String,[String])
assignmentAndText = do
   before <- try getInput
   asm    <- assignment
   after  <- getInput
   let blen = length before
       alen = length after
       text = dropWhileEnd isSpace $ take (blen - alen) before
   return (asm,text,lines after)

spanProduction :: [ String ] -> Either ParseError (Assignment,String,[String])
spanProduction = parse assignmentAndText "" . nextProduction

productions :: [String] -> [(Assignment,String)]
productions [] = []
productions ls@(_:xs) =
    case spanProduction ls of
        Right (asm,text,ls') -> (asm,text) : productions ls'
        Left _               -> productions xs

