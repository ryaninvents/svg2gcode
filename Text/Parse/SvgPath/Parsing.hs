module Text.Parse.SvgPath.Parsing
( svgPath
, p_moveto_drawto_command_group
, p_drawto_command
, p_moveto
, p_lineto
, p_closepath
, p_comma_wsp
, p_coordinate_pair
, p_float
, p_number
, PathInstruction
  ( MoveTo
  , LineTo
  , HorizontalLineTo
  , VerticalLineTo
  , CurveTo
  , SmoothCurveTo
  , QuadraticBezierCurveTo
  , SmoothQuadraticBezierCurveTo
  , EllipticalArcTo
  , ClosePath
  )
, Path
, Coordinates
) where

import Control.Applicative hiding (many, optional, (<|>))
import Text.ParserCombinators.Parsec
import Numeric (readSigned, readFloat)

data PathInstruction = MoveTo Bool Coordinates
                     | LineTo Bool Coordinates
                     | HorizontalLineTo Bool Double
                     | VerticalLineTo Bool Double
                     | CurveTo Bool Coordinates Coordinates Coordinates
                     | SmoothCurveTo Bool Coordinates Coordinates
                     | QuadraticBezierCurveTo Bool Coordinates Coordinates
                     | SmoothQuadraticBezierCurveTo Bool Coordinates
                     | EllipticalArcTo Coordinates Double Bool Bool Coordinates
                     | ClosePath
                     deriving (Show, Eq)

type Path = [PathInstruction]
type Coordinates = (Double, Double)

-- Parser modeled after http://www.w3.org/TR/SVG/paths.html#PathDataBNF
svgPath :: CharParser () Path
svgPath = do spaces 
             paths <- p_moveto_drawto_command_group `sepEndBy` spaces
             eof
             return $ concat paths

p_moveto_drawto_command_group :: CharParser () Path
p_moveto_drawto_command_group = do m <- p_moveto
                                   spaces
                                   ds <- p_drawto_command `sepEndBy` spaces
                                   return $ m ++ (concat ds)

p_drawto_command :: CharParser () Path
p_drawto_command = choice [ p_lineto <?> "LineTo"
                          , p_closepath <?> "ClosePath"
                          ]

p_moveto :: CharParser () Path
p_moveto = do command <- oneOf "Mm" <* spaces
              let isRelative = command == 'm'
                  mkLineto (x, y) = LineTo isRelative (x, y)
              coords <- p_coordinate_pair `sepEndBy` p_comma_wsp
              let (x, y) = head coords
                  move = MoveTo isRelative (x, y)
              return $ move:(map mkLineto (tail coords))

p_lineto :: CharParser () Path
p_lineto = do command <- oneOf "Ll" <* spaces
              let isRelative = command == 'l'
                  mkLineto (x, y) = LineTo isRelative (x, y)
              coords <- p_coordinate_pair `sepEndBy` p_comma_wsp
              let (x, y) = head coords
                  line = LineTo isRelative (x, y)
              return $ line:(map mkLineto (tail coords))

p_closepath :: CharParser () Path
p_closepath = oneOf "Zz" >> return [ClosePath]

p_comma_wsp :: CharParser () ()
p_comma_wsp = try (spaces >> char ',' >> spaces)
           <|> skipMany1 space

p_coordinate_pair :: CharParser () Coordinates
p_coordinate_pair = do
  x <- p_number
  p_comma_wsp
  y <- p_number
  return (x, y)

p_float :: CharParser () Double
p_float = do s <- getInput
             case readSigned readFloat s of
               [(n, s')] -> n <$ setInput s'
               _         -> empty

p_number :: CharParser () Double
p_number = (optional $ char '+') *> (p_float <?> "number")
