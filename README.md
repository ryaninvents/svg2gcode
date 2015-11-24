# svg2gcode

Just what it says on the label: converts SVG files into Gcode that a 3D printer can consume.

## Usage

```
cabal configure
cabal install
cabal build

# Outputs an executable to dist/build/svg2gcode/svg2gcode
dist/build/svg2gcode/svg2gcode < samples/haskell.svg > ~/Documents/haskell.gcode
```

Then attach a Sharpie to the print head, send the gcode to your 3D printer, and it will draw the SVG. The Sharpie should touch the paper at `Z = 0` and should not touch the paper at `Z = 1` in order for it to draw/move properly.

## License

See [LICENSE](LICENSE).
