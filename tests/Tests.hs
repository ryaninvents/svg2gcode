import Test.Hspec
import Test.QuickCheck

import Text.ParserCombinators.Parsec
import Text.Parse.SvgPath.Parsing

import Control.Applicative ((<*))

main :: IO ()
main = hspec $ do
  describe "p_number" $ do
    it "parses an integer" $ do
      (parse p_number "error" "345") `shouldBe` (Right 345.0)
    it "parses a negative float" $ do
      (parse p_number "error" "-0.2118") `shouldBe` (Right (-0.2118))
    it "parses an explicitly positive float" $ do
      (parse p_number "error" "+0.2118") `shouldBe` (Right 0.2118)
    it "parses a negative float with exponent" $ do
      let en = parse p_number "error" "-3.14e-5"
      en `shouldBe` (Right (-3.14e-5))
  describe "p_coordinate_pair" $ do
    it "parses a comma-separated pair" $ do
      let en = parse p_coordinate_pair "error" "345.6,789.0"
      en `shouldBe` (Right (345.6, 789.0))
    it "parses a space-separated pair" $ do
      let en = parse p_coordinate_pair "error" "345.6 789.0"
      en `shouldBe` (Right (345.6, 789.0))
    it "parses a comma-separated pair of negatives" $ do
      (parse p_coordinate_pair "error" "-0.567,-0.234") `shouldBe` (Right ((-0.567),(-0.234)))
  describe "p_comma_wsp" $ do
    it "parses a single comma" $ do
      (parse p_comma_wsp "error" ",") `shouldBe` (Right ())
    it "parses a comma with spaces on the left" $ do
      (parse p_comma_wsp "error" "   ,") `shouldBe` (Right ())
    it "parses a comma with spaces on the right" $ do
      (parse p_comma_wsp "error" ",   ") `shouldBe` (Right ())
    it "parses a single space" $ do
      (parse p_comma_wsp "error" " ") `shouldBe` (Right ())
    it "parses multiple spaces" $ do
      (parse p_comma_wsp "error" "    ") `shouldBe` (Right ())
    it "can separate space-separated coordinate pairs by whitespace" $ do
      (parse (p_coordinate_pair `sepBy` p_comma_wsp) "error" "5 6 7 8") `shouldBe` (Right [ (5.0, 6.0)
                                                                                          , (7.0, 8.0)
                                                                                          ])
    it "can separate space-separated coordinate pairs by comma" $ do
      (parse (p_coordinate_pair `sepBy` p_comma_wsp) "error" "5 6,7 8") `shouldBe` (Right [ (5.0, 6.0)
                                                                                          , (7.0, 8.0)
                                                                                          ])
    it "can separate comma-separated coordinate pairs by whitespace" $ do
      (parse (p_coordinate_pair `sepBy` p_comma_wsp) "error" "5,6 7,8") `shouldBe` (Right [ (5.0, 6.0)
                                                                                          , (7.0, 8.0)
                                                                                          ])
    it "can separate comma-separated coordinate pairs by comma" $ do
      (parse (p_coordinate_pair `sepBy` p_comma_wsp) "error" "5,6,7,8") `shouldBe` (Right [ (5.0, 6.0)
                                                                                          , (7.0, 8.0)
                                                                                          ])
  describe "p_moveto" $ do
    describe "no space after command" $ do
      it "parses moveto with comma-separated coords" $ do
        (parse p_moveto "error" "M5,6") `shouldBe` (Right [MoveTo False (5.0,6.0)])
      it "parses moveto with space-separated coords" $ do
        (parse p_moveto "error" "M5 6") `shouldBe` (Right [MoveTo False (5.0,6.0)])
      it "handles implicit following lineto" $ do
        (parse p_moveto "error" "M5,6 7,8") `shouldBe` (Right [ MoveTo False (5.0, 6.0)
                                                              , LineTo False (7.0, 8.0)
                                                              ])
    describe "with space after command" $ do
      it "parses moveto space-separated coords" $ do
        (parse p_moveto "error" "M 5 6") `shouldBe` (Right [MoveTo False (5.0,6.0)])
      it "parses moveto comma-separated coords" $ do
        (parse p_moveto "error" "M 5,6") `shouldBe` (Right [MoveTo False (5.0,6.0)])
      it "handles implicit following lineto" $ do
        (parse p_moveto "error" "M 5,6 7,8") `shouldBe` (Right [ MoveTo False (5.0, 6.0)
                                                               , LineTo False (7.0, 8.0)
                                                               ])
  describe "p_lineto" $ do
    describe "no space after command" $ do
      it "parses moveto with comma-separated coords" $ do
        (parse p_lineto "error" "L5,6") `shouldBe` (Right [LineTo False (5.0,6.0)])
      it "parses moveto with space-separated coords" $ do
        (parse p_lineto "error" "L5 6") `shouldBe` (Right [LineTo False (5.0,6.0)])
      it "handles implicit following lineto" $ do
        (parse p_lineto "error" "L5,6 7,8") `shouldBe` (Right [ LineTo False (5.0, 6.0)
                                                              , LineTo False (7.0, 8.0)
                                                              ])
    describe "with space after command" $ do
      it "parses moveto space-separated coords" $ do
        (parse p_lineto "error" "L 5 6") `shouldBe` (Right [LineTo False (5.0,6.0)])
      it "parses moveto comma-separated coords" $ do
        (parse p_lineto "error" "L 5,6") `shouldBe` (Right [LineTo False (5.0,6.0)])
      it "handles implicit following lineto" $ do
        (parse p_lineto "error" "L 5,6 7,8") `shouldBe` (Right [ LineTo False (5.0, 6.0)
                                                               , LineTo False (7.0, 8.0)
                                                               ])

  describe "p_moveto_drawto_command_group" $ do
      it "parses just a moveto" $ do
        (parse p_moveto_drawto_command_group "error" "M 5,6") `shouldBe` (Right [MoveTo False (5.0,6.0)])
      it "parses a moveto followed by a lineto" $ do
        (parse p_moveto_drawto_command_group "error" "M 5,6 L 7,8") `shouldBe` (Right [ MoveTo False (5.0,6.0)
                                                                                      , LineTo False (7.0,8.0)
                                                                                      ])
      it "parses a moveto followed by multiple linetos" $ do
        (parse p_moveto_drawto_command_group "error" "M 5,6 L 7,8 L 9,10") `shouldBe` (Right [ MoveTo False (5.0,6.0)
                                                                                             , LineTo False (7.0,8.0)
                                                                                             , LineTo False (9.0,10.0)
                                                                                             ])
      it "parses a moveto, an implicit lineto, and an explicit lineto" $ do
        (parse p_moveto_drawto_command_group "error" "M 5,6 9,10 L 7,8") `shouldBe` (Right [ MoveTo False (5.0, 6.0)
                                                                                           , LineTo False (9.0, 10.0)
                                                                                           , LineTo False (7.0, 8.0)
                                                                                           ])
