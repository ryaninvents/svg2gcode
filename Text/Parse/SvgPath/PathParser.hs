module Text.Parse.SvgPath.PathParser
( Path
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
, parsePath
, pathBounds
, instructionBounds
, convertToAbsolute
, scaleToFit
, dumbGcode
) where

import Control.Applicative hiding (many, optional, (<|>))
import Text.ParserCombinators.Parsec
import Numeric (readSigned, readFloat)

import Text.Parse.SvgPath.Parsing

parsePath :: String -> Either ParseError Path
parsePath str = parse svgPath "SVG path parse error" str

-- ((xmin, ymin), (xmax, ymax))
type Bounds = (Coordinates, Coordinates)

instructionBounds :: Coordinates -> PathInstruction -> Bounds
instructionBounds (x, y) (MoveTo rel (x', y')) = let x'' = x + x'
                                                     y'' = y + y'
                                                 in case rel of True -> ((min x x'', min y y''), (max x x'', max y y''))
                                                                False -> ((min x x', min y y'),  (max x x',  max y y' ))

instructionBounds (x, y) (LineTo rel (x', y')) = let x'' = x + x'
                                                     y'' = y + y'
                                                 in case rel of True -> ((min x x'', min y y''), (max x x'', max y y''))
                                                                False -> ((min x x', min y y'),  (max x x',  max y y' ))

-- To be implemented
instructionBounds pt _ = (pt, pt)

convertToAbsolute :: Path -> Path
convertToAbsolute p = newPath
  where newPath = convertToAbsolute' (0.0, 0.0) p' p'
        p' = trimExtraMoves p

trimExtraMoves [] = []
trimExtraMoves (x:xs) = xs'
  where xs' = trimExtra x xs
        trimExtra x [] = [x]
        trimExtra (MoveTo _ _) (c@(MoveTo _ _):xs) = trimExtra c xs
        trimExtra c (x:xs) = c:(trimExtra x xs)

-- Convert to absolute coordinates, starting at the given origin and using the given
-- moveto-command-group. Outputs the transformed path
convertToAbsolute' :: Coordinates -> Path -> Path -> Path

-- For absolute commands, just pass them through.
-- A MoveTo signifies the start of a new moveto-command-group.
convertToAbsolute' _ _   (c@(MoveTo False p):cs) = c:(convertToAbsolute' p [c] cs)
convertToAbsolute' _ mcg (c@(LineTo False p):cs) = c:(convertToAbsolute' p mcg' cs)
  where mcg' = mcg ++ [c]

-- Relative commands need to be converted.
convertToAbsolute' (x,y) _ (c@(MoveTo True (x',y')):cs) = c':(convertToAbsolute' p' [c'] cs)
  where p' = (x+x', y+y')
        c' = MoveTo False p'
convertToAbsolute' (x,y) mcg (c@(LineTo True (x',y')):cs) = c':(convertToAbsolute' p' mcg' cs)
  where p' = (x+x', y+y')
        c' = LineTo False p'
        mcg' = mcg ++ [c']

-- For ClosePath, convert it to a draw command.
convertToAbsolute' _ mcg@((MoveTo False p):_) (ClosePath:cs) = (LineTo False p):(convertToAbsolute' p mcg' cs)
  where mcg' = mcg ++ [ClosePath]

-- Anything else, drop from the output.
convertToAbsolute' p mcg (_:cs) = convertToAbsolute' p mcg cs
-- Empty case.
convertToAbsolute' _ _ [] = []

pathBounds :: Path -> Bounds
pathBounds p = foldl maxBounds ((0.0,0.0),(0.0,0.0)) bounds
  where bounds = map (instructionBounds (0.0,0.0)) p
        maxBounds ((xmin,ymin),(xmax,ymax)) ((xmin', ymin'), (xmax', ymax')) = ((min xmin xmin',min ymin ymin'),
                                                                                (max xmax xmax',max ymax ymax'))

-- Given a bounding box, scale the given path proportionally so that
-- it is centered within the bounding box.
scaleToFit :: Bounds -> Path -> Path
scaleToFit target [] = []
scaleToFit target cs = scaleToFit' (boundsScaleAndCenter target (pathBounds cs)) cs

scaleToFit' :: (Coordinates -> Coordinates) -> Path -> Path
scaleToFit' _ [] = []
scaleToFit' s (c:cs) = (scale c):(scaleToFit' s cs)
  where scale (MoveTo rel coords) = MoveTo rel (s coords)
        scale (LineTo rel coords) = LineTo rel (s coords)
        scale (HorizontalLineTo rel x) = let (x', _) = s (x, 0)
                                          in HorizontalLineTo rel x'
        scale (VerticalLineTo rel y) = let (_, y') = s (0, y)
                                        in VerticalLineTo rel y'
        scale (CurveTo rel ca cb cc) = CurveTo rel (s ca) (s cb) (s cc)
        scale (SmoothCurveTo rel ca cb) = SmoothCurveTo rel (s ca) (s cb)
        scale (QuadraticBezierCurveTo rel ca cb) = QuadraticBezierCurveTo rel (s ca) (s cb)
        scale (SmoothQuadraticBezierCurveTo rel ca) = SmoothQuadraticBezierCurveTo rel (s ca)
        scale (EllipticalArcTo cr ro lg sw ca) = EllipticalArcTo (s cr) ro lg sw (s ca)
        scale ClosePath = ClosePath

dumbGcode (MoveTo _ (x, y)) = "G0 Z1\nG0 X" ++ (show x) ++ " Y" ++ (show y)
dumbGcode (LineTo _ (x, y)) = "G0 Z0\nG0 X" ++ (show x) ++ " Y" ++ (show y)
dumbGcode _ = ""

boundsScaleAndCenter :: Bounds -> Bounds -> Coordinates -> Coordinates
boundsScaleAndCenter target@((xmin',ymin'),(xmax',ymax')) original@((xmin,ymin),(xmax,ymax)) =
  let sx = getScale xmin xmax xmin' xmax'
      sy = getScale ymin ymax ymin' ymax'
      s = min sx sy
      cx = getCenter s xmin xmin'
      cy = getCenter s ymin ymin'
   in scaleCoord s cx cy

scale s xc x = s*x + (1-s) * xc
scaleCoord s xc yc (x,y) = (x',y')
  where x' = scale s xc x
        y' = scale s yc y

getCenter s x x' = (x' - s*x) / (1.0 - s)

getScale xmin xmax xmin' xmax' = xspan' / xspan
  where xspan = xmax - xmin
        xspan' = xmax' - xmin'
