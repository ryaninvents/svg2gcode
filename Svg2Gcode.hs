module Main where

import Control.Arrow
import Text.XML.HXT.Core

data PathInstruction = Move Double Double Double
                     | StartPath
                     | EndPath

getDoc = readDocument [] ""

main :: IO ()
main = do
  result <- runX (getDoc >>> selectAllPaths)
  case result of
    [] -> putStrLn "No paths found"
    p:_ -> putStrLn (take 30 p)

selectAllPaths :: ArrowXml a => a XmlTree String
selectAllPaths = deep
                 ( isElem >>> hasName "path"
                   >>>
                   getAttrValue "d"
                 )
