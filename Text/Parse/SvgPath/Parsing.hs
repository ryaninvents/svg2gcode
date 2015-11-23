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
                     | SmoothQuadraticBezierCurveTo Bool Coordinates Coordinates
                     | EllipticalArc Double Double Double Bool Bool Coordinates
                     | ClosePath
                     deriving (Show, Eq)

type Path = [PathInstruction]
type Coordinates = (Double, Double)

-- Parser modeled after http://www.w3.org/TR/SVG/paths.html#PathDataBNF
svgPath :: CharParser () Path
svgPath = do spaces 
             paths <- p_moveto_drawto_command_group `sepBy` spaces
             spaces
             eof
             return $ concat paths

p_moveto_drawto_command_group :: CharParser () Path
p_moveto_drawto_command_group = do m <- p_moveto
                                   spaces
                                   ds <- option [] $ p_drawto_command `sepBy` spaces
                                   return $ m ++ (concat ds)

p_drawto_command :: CharParser () Path
p_drawto_command = choice [ p_lineto <?> "LineTo"
                          , p_closepath <?> "ClosePath"
                          ]

p_moveto :: CharParser () Path
p_moveto = do command <- oneOf "Mm" <?> "move command"
              let isRelative = command == 'm'
                  mkLineto (x, y) = LineTo isRelative (x, y)
              spaces
              (x, y) <- p_coordinate_pair
              ls <- (optional p_comma_wsp) *> (p_coordinate_pair `sepBy` p_comma_wsp)
              let ls' = map mkLineto ls
              return $ [MoveTo isRelative (x, y)] ++ ls'

p_lineto :: CharParser () Path
p_lineto = do command <- oneOf "Ll"
              let isRelative = command == 'l'
                  mkLineto (x, y) = LineTo isRelative (x, y)
              spaces
              (x, y) <- p_coordinate_pair
              ls <- option [] $ (optional p_comma_wsp) *> (p_coordinate_pair `sepBy` p_comma_wsp)
              let ls' = map mkLineto ls
              return $ [LineTo isRelative (x, y)] ++ ls'

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
p_number = (char '+' <|> pure ' ') *> p_float
