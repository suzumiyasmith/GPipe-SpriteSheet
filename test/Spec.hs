import Control.Monad
import Control.Monad.IO.Class
import Control.Exception.Base
import Control.Exception

import Control.Concurrent
-- import Control.Concurrent.Async
import Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW

import Data.Monoid
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as B

import Graphics.GPipe.SpriteSheet

import Control.Monad.State

import System.Random

import Data.IORef

main :: IO ()
main = do
  mainTest

mainTest :: IO ()
mainTest = do
  m <- newIORef []
  forkOS $ mainRender (readIORef m)
  liftIO $ print "ready"
  (`catch` (\(e :: SomeException) -> print e)) $ void $ (`runStateT` 0) $ forever $ do
    ss :: Float <- (get :: StateT Float IO Float)
    put (ss + 0.01)
    (lift :: IO () -> StateT Float IO ()) $ do
      sn <- randomRIO (1, 54 :: Int)
      -- px <- randomRIO (0 :: Float, 400)
      -- py <- randomRIO (0 :: Float, 400)
      -- putMVar m [("sprite" <> show (2 :: Int), (V2 (150 * sin (10 * ss)) (150 * cos (10 * ss)), V2 0.001 0.001, 0))]
      -- tryPutMVar m $ fmap ((,) $ "sprite" <> show (2 :: Int)) $ bullets ss <$> fromIntegral <$> [0..(floor $ 10 * ss)]
      forkIO $ writeIORef m $ fmap ((,) $ "sprite" <> show (2 :: Int)) $ bullets ss <$> fromIntegral <$> [0..(floor $ 10 * ss)]
      -- forkIO $ writeIORef m $ fmap ((,) $ "sprite" <> show (2 :: Int)) $ testPosition <$> [1,2]
      threadDelay (16 * 1000)

  putStrLn "over"

bullets t i =
  (V2
    (log (1 + t/i) * 150 * (1 / i + 1) * sin ((i) * t))
    (log (1 + t/i) * 150 * (1 / i + 1) * cos ((i) * t))
  , pure (2/(sqrt i))
  , t * pi / 2)

testPosition 1 = (V2 0 0, V2 1 1, pi/4)
testPosition 2 = (V2 (1920) ((-1080)), V2 1 1, 0)
testPosition 3 = (V2 0 1, V2 1 1, 0)

test1 :: IO ()
test1 = void $ (`runStateT` 0) $ forever $ do
  ss :: Float <- get
  put (ss + 1)
  liftIO $ print "start ~!!"
  (liftIO :: IO () -> StateT Float IO ()) $ do
    print ss
    threadDelay 1600

-- test :: IO (Either String (V.Vector (String, Int, Int, Int, Int)))
-- test = C.decode C.NoHeader <$> LB.readFile "sprites.txt"

mainRender :: (IO [(String, (V2 Float, V2 Float, Float))]) -> IO ()
mainRender r = void $ do
  sh <- liftIO $ LB.readFile "sprites.txt"
  pic <- liftIO $ B.readFile "spritesheet.png"
  runContextT GLFW.defaultHandleConfig $ do
    win <- newWindow (WindowFormatColor RGBA8) windowConfig
    renderDevice win (V2 1920 1080) (V4 0 0 0 0) (readSprites sh pic) 10000000 r

windowConfig :: GLFW.WindowConfig
windowConfig = GLFW.WindowConfig 1920 1080 "Hola" Nothing [] Nothing
