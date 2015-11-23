module Main where

import Control.Arrow
import System.IO (hPutStrLn, stderr)
import Text.XML.HXT.Core
import Text.ParserCombinators.Parsec
import Data.List (intercalate)

import Text.Parse.SvgPath.PathParser (Path, PathInstruction, parsePath)

getDoc = readDocument [] ""

main :: IO ()
main = do
  result <- runX (getDoc >>> selectAllPaths)
  case result of
    []  -> putStrLn "No paths found"
    p:_ -> case parsePath p of
             Left err -> putErrLn $ "ERR\n" ++ p ++ "\n" ++ (replicate n ' ') ++ "^\n" ++ message
               where pos = errorPos err
                     n = (sourceColumn pos) - 1
                     nSkip = max 0 (n - 60)
                     nSpace = min n 60
                     message = show err
                     p' = take (n + 10) p
                     putErrLn = hPutStrLn stderr
             Right p  -> putStrLn $ show p

selectAllPaths :: ArrowXml a => a XmlTree String
selectAllPaths = deep
                 ( isElem >>> hasName "path"
                   >>>
                   getAttrValue "d"
                 )

