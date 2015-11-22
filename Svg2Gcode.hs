module Main where

import Control.Arrow
import Text.XML.HXT.Arrow.ReadDocument (readDocument)
import Text.XML.HXT.Arrow.WriteDocument (writeDocument)
import Text.XML.HXT.Arrow.XmlState (withIndent)
import Text.XML.HXT.Arrow.XmlState.RunIOStateArrow (runX)

main :: IO ()
main = do
  runX $
    readDocument [] ""
    >>>
    writeDocument [withIndent True] ""
  return ()
