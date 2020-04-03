module Plugin.Pic where

import Codec.Picture( PixelRGBA8( .. ), writePng )
import Graphics.Rasterific
import Graphics.Text.TrueType
import Graphics.Rasterific.Texture
import Utils.Logging

blackTexture = Just . uniformTexture $ PixelRGBA8 0 0 0 255
blueTexture = Just . uniformTexture $ PixelRGBA8 0 0 255 255
redTexture =   Just . uniformTexture $ PixelRGBA8 255 0 0 255

drawTextArray :: [[(String, FontDescrb)]] -> IO ()
drawTextArray texts = do
  font_FZHeiTi <- loadFontFile $ slcFont FZHeiTi
  font_MSYaHei <- loadFontFile $ slcFont MSYaHei
  case font_FZHeiTi of
    Left err -> logErr "Drawing pic" err
    Right fzHeiTi ->
      case font_MSYaHei of
        Left err -> logErr "Drawing pic" err
        Right msYaHei ->
          let makeTextList cnt (x:xs) = (printTextRanges (V2 5 (22*cnt)) $
                fmap (\(text, (FontDescrb color font)) ->
                  TextRange (case font of
                               FZHeiTi -> fzHeiTi
                               MSYaHei -> msYaHei)
                    (PointSize 16) text (slcColor color)) x) : (makeTextList (cnt+1) xs)
              makeTextList _ [] = [] in
          writePng "text.png" .
            renderDrawing 2000 (11 + 22 * length texts) (PixelRGBA8 255 255 255 255) .
              withTexture (uniformTexture $ PixelRGBA8 0 0 0 255) $ do
                foldr (>>) (pure ()) (makeTextList 1 texts)


data FontDescrb = FontDescrb FontColor FontType

data FontColor = FontRed | FontBlack | FontBlue
data FontType  = FZHeiTi | MSYaHei

slcColor FontRed = redTexture
slcColor FontBlue = blueTexture
slcColor FontBlack = blackTexture
slcFont FZHeiTi = "fonts/fz_hei_ti.ttf"
slcFont MSYaHei = "fonts/ms_ya_hei.ttf"
