module Plugin.Pic where

import Codec.Picture( PixelRGBA8( .. ), writePng )
import Graphics.Rasterific
import Graphics.Text.TrueType        (Font, loadFontFile )
import Graphics.Rasterific.Texture
import Utils.Logging

picTest :: IO ()
picTest = do
  let white = PixelRGBA8 255 255 255 255
      drawColor = PixelRGBA8 0 0x86 0xc1 255
      recColor = PixelRGBA8 0xFF 0x53 0x73 255
      img = renderDrawing 400 200 white $
         withTexture (uniformTexture drawColor) $ do
            fill $ circle (V2 0 0) 30
            stroke 4 JoinRound (CapRound, CapRound) $
                   circle (V2 400 200) 40
            withTexture (uniformTexture recColor) .
                   fill $ rectangle (V2 100 100) 200 100

  writePng "yourimage.png" img

drawTextArray :: [String] -> IO ()
drawTextArray texts = do
  fontErr <- loadFontFile "deng.ttf"
  case fontErr of
    Left err -> logErr "Drawing pic" err
    Right font ->
      writePng "text.png" .
        renderDrawing 2000 (11 + 22 * length texts) (PixelRGBA8 255 255 255 255) .
          withTexture (uniformTexture $ PixelRGBA8 0 0 0 255) $ do
            foldr (>>) (pure ()) (makeTextList texts 1 font)

makeTextList :: [String] -> Float -> Font -> [Drawing px ()]
makeTextList (x:xs) cnt font = (printTextAt font (PointSize 16) (V2 0 (22*cnt)) x) : (makeTextList xs (cnt+1) font)
makeTextList [] _ _ = []
