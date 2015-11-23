module Text.Parse.SvgPath.PathParser (
  Path,
  PathInstruction,
  parsePath
) where

import Control.Applicative hiding (many, optional, (<|>))
import Text.ParserCombinators.Parsec
import Numeric (readSigned, readFloat)

import Text.Parse.SvgPath.Parsing

parsePath :: String -> Either ParseError Path
parsePath str = parse svgPath "SVG path parse error" str
