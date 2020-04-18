module Module.Pic where

import Codec.Picture (PixelRGBA8( .. ), decodePng, encodePng, decodePng)
import Codec.Picture.Saving (imageToJpg)
import qualified Data.ByteString.Lazy as BL
import Graphics.Rasterific
import Graphics.Text.TrueType
import Graphics.Rasterific.Texture
import Utils.Logging
import Data.Foldable

blackTexture :: Maybe (Texture PixelRGBA8)
blackTexture = Just . uniformTexture $ PixelRGBA8 0 0 0 255

blueTexture :: Maybe (Texture PixelRGBA8)
blueTexture = Just . uniformTexture $ PixelRGBA8 0 0 255 255

redTexture :: Maybe (Texture PixelRGBA8)
redTexture = Just . uniformTexture $ PixelRGBA8 255 0 0 255

drawTextArray :: String -> Int -> [[DrawText]] -> IO ()
drawTextArray path quality texts = do
  font_FZHeiTi <- loadFontFile $ slcFont FZHeiTi
  font_MSYaHei <- loadFontFile $ slcFont MSYaHei
  case font_FZHeiTi of
    Left err -> logErr "Drawing pic" err
    Right fzHeiTi ->
      case font_MSYaHei of
        Left err -> logErr "Drawing pic" err
        Right msYaHei -> do
          let makeTextList cnt (x:xs) = printTextRanges (V2 5 (22*cnt)) (
                fmap (\(DrawText text (FontDescrb color font)) ->
                  TextRange (case font of
                               FZHeiTi -> fzHeiTi
                               MSYaHei -> msYaHei)
                    (PointSize 16) text (slcColor color)) x) : makeTextList (cnt+1) xs
              makeTextList _ [] = []
          let png = decodePng.BL.toStrict . encodePng $
                     renderDrawing 1800 (11 + 22 * length texts) (PixelRGBA8 255 255 255 255) .
                       withTexture (uniformTexture $ PixelRGBA8 0 0 0 255) $
                         foldr (>>) (pure ()) (makeTextList 1 texts)
          traverse_ (BL.writeFile path .imageToJpg quality) png

data DrawText = DrawText String FontDescrb
  deriving (Show)

data FontDescrb = FontDescrb FontColor FontType
  deriving (Show)

data FontColor = FontRed | FontBlack | FontBlue
  deriving (Show)
data FontType  = FZHeiTi | MSYaHei
  deriving (Show)

slcColor :: FontColor -> Maybe (Texture PixelRGBA8)
slcColor FontRed = redTexture
slcColor FontBlue = blueTexture
slcColor FontBlack = blackTexture

slcFont :: FontType -> String
slcFont FZHeiTi = "fonts/fz_hei_ti.ttf"
slcFont MSYaHei = "fonts/ms_ya_hei.ttf"
