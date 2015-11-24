module Main where

import Control.Arrow
import System.IO (hPutStrLn, stderr)
import Text.XML.HXT.Core
import Text.ParserCombinators.Parsec
import Data.List (intercalate)

import Text.Parse.SvgPath.PathParser
  ( Path
  , PathInstruction
  , parsePath
  , convertToAbsolute
  , pathBounds
  , scaleToFit
  , dumbGcode
  )

getDoc = readDocument [] ""

printBed = ((-90.0,-90.0),(90.0,90.0))

main :: IO ()
main = do
  result <- runX (getDoc >>> selectAllPaths)
  let nonZeroLength [] = False
      nonZeroLength _ = True
      pathFromEither p = case parsePath p of Left err -> []
                                             Right p -> p
      paths = filter nonZeroLength $ map pathFromEither result
      path = concat $ map convertToAbsolute paths
      bounds = pathBounds path
  let path' = scaleToFit printBed path
  putStrLn $ "; New bounds " ++ (show $ pathBounds path')
  putStrLn $ intercalate "\n" (map dumbGcode path')

selectAllPaths :: ArrowXml a => a XmlTree String
selectAllPaths = deep
                 ( isElem >>> hasName "path"
                   >>>
                   getAttrValue "d"
                 )

