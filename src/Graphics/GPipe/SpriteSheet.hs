module Graphics.GPipe.SpriteSheet where

import Control.Arrow
import Control.Monad
import Control.Monad.State
import Control.Monad.IO.Class

import qualified Data.ByteString as B
import Codec.Picture
import Linear
import Data.List.Split

import qualified Data.Vector as V
import qualified Data.Vector.Storable as S
import Data.Word

import qualified Data.Csv as C
import qualified Data.ByteString.Lazy as LB

import qualified Data.Map as Map

import Graphics.GPipe

procPicture :: B.ByteString -> (Format RGBAFloat, V2 Int, [V4 Float])
procPicture raw = (RGBA8, V2 w h, dd)  where
  Right dPic = decodePng raw
  Image w h ds = convertRGBA8 dPic
  dd =
    fmap (\[r, g, b, a] -> V4 r g b a) $
    chunksOf 4 $
    (\s -> fromIntegral s / fromIntegral (maxBound :: Word8)) <$>
    S.toList ds

procSheet :: LB.ByteString -> V2 Int -> Map.Map String [(V2 Float, V2 Float)]
procSheet sh (V2 pw ph) = Map.fromList $ ff <$> V.toList r where
  Right r = C.decode C.NoHeader sh :: Either String (V.Vector (String, Int, Int, Int, Int))
  ff (s, x, y, w, h) = (s,) $ zip (fmap fromIntegral <$> box (-w) w (-h) h) ((\(V2 x y) -> V2 (fromIntegral x / fromIntegral pw) (fromIntegral y / fromIntegral ph)) <$> box x (x+w) y (y+h))
  box x0 x1 y0 y1 = [V2 x0 y0, V2 x1 y0, V2 x0 y1, V2 x1 y0, V2 x0 y1, V2 x1 y1]

readSprites :: LB.ByteString -> B.ByteString -> ((Format RGBAFloat, V2 Int, [V4 Float]), Map.Map String [(V2 Float, V2 Float)])
readSprites sh pic = ((f, wh, d), r) where
  (f, wh, d) = procPicture pic
  r = procSheet sh wh

renderDevice ::
     ContextHandler ctx
  => Window os RGBAFloat ds
  -> V2 Int
  -> V4 Float
  -> ((Format RGBAFloat, V2 Int, [V4 Float]), Map.Map String [(V2 Float, V2 Float)])
  -> Int
  -> IO [(String, (V2 Float, V2 Float, Float))]
  -> ContextT ctx os IO ()
renderDevice win winSize clearColor (pic, spriteSheet) bufferLimit displayData = do
  tex1 <- initTexture2D pic
  p1Shader <- compileShader $ playerShader tex1 win winSize
  buf <- newBuffer bufferLimit

  forever $ do
    bs <- liftIO displayData
    writeBuffer buf 0 $ concat $ (\(k, t) -> first (transferSprite winSize t) <$> spriteSheet Map.! k) <$> bs

    render $ do
      clearWindowColor win clearColor
      va <- newVertexArray buf
      let pa = toPrimitiveArray TriangleList $ takeVertices (6 * length bs) va
      p1Shader pa
    swapWindowBuffers win

transferSprite :: V2 Int -> (V2 Float, V2 Float, Float) -> V2 Float -> V2 Float
-- transferSprite (p, s, r) v = rotateMatrix2D r !* (s * (v + p)) where
transferSprite winSize (p, s, r) = (/ (fromIntegral <$> winSize)) . (p +) . (s *) . (rotateMatrix2D r !*) where
  rotateMatrix2D r = V2 (V2 (cos r) (sin r)) (V2 (- sin r) (cos r))

initTexture2D ::
     ( ContextHandler ctx
     , MonadIO m
     , BufferFormat b
     , ColorSampleable c
     , BufferColor (Color c (ColorElement c)) h ~ b
     , h ~ HostFormat b
     )
  => (Format c, Size2, [h])
  -> ContextT ctx os m (Texture2D os (Format c))
initTexture2D (format, wh, d) = do
  tex <- newTexture2D format wh 1
  writeTexture2D tex 0 0 wh d
  return tex

playerShader :: Texture2D os (Format RGBAFloat)
  -> Window os RGBAFloat ds
  -> V2 Int
  -> Shader os (PrimitiveArray Triangles (B2 Float, B2 Float)) ()
playerShader tex win winSize = do
  primitiveStream <- toPrimitiveStream id
  let primitiveStreamAll :: PrimitiveStream Triangles (V4 VFloat, V2 VFloat)
      primitiveStreamAll = first toV4 <$> primitiveStream
  fragmentStream <-
    rasterize
      (const (FrontAndBack, ViewPort (V2 0 0) winSize, DepthRange 0 1))
      primitiveStreamAll
  let filter = SamplerFilter Nearest Nearest Nearest Nothing
      edge = (pure Repeat, undefined)
  samp <- newSampler2D (const (tex, filter, edge))
  let
    sampleTexture :: V2 FFloat -> V4 FFloat
    sampleTexture = sample2D samp SampleAuto Nothing Nothing
    fragmentStream2 = fmap sampleTexture fragmentStream
  drawWindowColor
    (const (win, ContextColorOption simpleBlender (pure True)))
    fragmentStream2

toV4 :: V2 VFloat -> V4 VFloat
toV4 (V2 x y) = V4 x y 0 1

simpleBlender :: Blending
simpleBlender =
  BlendRgbAlpha
    (FuncAdd, FuncAdd)
    (BlendingFactors SrcAlpha OneMinusSrcAlpha, BlendingFactors One Zero)
    (V4 0 0 0 0)
