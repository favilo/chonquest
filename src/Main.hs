module Main where

import Data.Word
import Data.Array.IArray

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader

import Graphics.UI.SDL.Image
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF
import qualified Graphics.UI.SDL.TTF.General as TTFG

----------------------------------------------------------------

screenWidth = 640
screenHeight = 480
screenBpp = 32

type RectArray = Array Int Rect

data Clip = MouseOver
    | MouseOut
    | MouseDown
    | MouseUp
    deriving (Eq, Ord, Enum, Show)

data Button = Button { box :: Rect,
    clip :: Clip
}

data AppConfig = AppConfig 
    { screen       :: Surface
    , buttonSheet  :: Surface
    , clips        :: RectArray
    }

type AppState = StateT Button IO
type AppEnv   = ReaderT AppConfig AppState

button :: Int -> Int -> Int -> Int -> Button
button x y w h = Button { box=Rect x y w h, clip=MouseOut }

isInside :: Integral a => Rect -> a -> a -> Bool
isInside (Rect rx ry rw rh) x y = (x' > rx) && (x' < rx + rw)
        && (y' > ry) && (y' < ry + rh)
    where (x', y') = (fromIntegral x, fromIntegral y)

handleEvent :: Button -> Event -> Button
handleEvent button@Button {box=b} (MouseMotion x y _ _) = 
    button { clip=clip' }
  where 
    clip' = if isInside b x y then MouseOver else MouseOut
handleEvent button@Button {box=b,clip=c} 
  (MouseButtonDown x y ButtonLeft) = 
        button {clip=clip'}
  where
    clip' = if isInside b x y then MouseDown else c
handleEvent button@Button {box=b, clip=c} 
  (MouseButtonUp x y ButtonLeft) = 
        button {clip=clip'}
  where
    clip' = if isInside b x y then MouseUp else c

handleEvent button _ = button

showButton :: Button -> RectArray -> Surface -> Surface -> IO Bool
showButton Button {box=Rect x y _ _, clip=clip} clips buttonSheet
    screen =
        applySurface x y buttonSheet screen $ Just clipRect 
    where 
        clipRect = clips ! fromEnum clip

runLoop :: AppConfig -> Button -> IO ()
runLoop = evalStateT . runReaderT loop

loadImage :: String -> Maybe (Word8, Word8, Word8)-> IO Surface
loadImage filename colorKey = load filename >>= displayFormat >>= 
        setColorKey' colorKey

setColorKey' :: Maybe (Word8, Word8, Word8) -> Surface -> 
        IO Surface
setColorKey' Nothing s = return s
setColorKey' (Just (r, g, b)) surface = 
    (mapRGB . surfaceGetPixelFormat) surface r g b >>= 
        setColorKey surface [SrcColorKey] >> return surface

applySurface :: Int -> Int -> Surface -> Surface -> 
        Maybe Rect -> IO Bool
applySurface x y src dst clip = blitSurface src clip dst offset
    where offset = Just $ Rect x y 0 0 

initEnv :: IO AppConfig
initEnv = do
    screen <- setVideoMode screenWidth screenHeight screenBpp 
        [SWSurface]
    setCaption "Button Test" []

    buttonSheet   <- loadImage "button.png" $ 
        Just (0x00, 0xff, 0xff)

    return $ AppConfig screen buttonSheet clips
  where 
    clips = listArray (0,3) 
        [Rect { rectX=0, rectY=0, rectW=320, rectH=240 },
         Rect { rectX=320, rectY=0, rectW=320, rectH=240 },
         Rect { rectX=0, rectY=240, rectW=320, rectH=240 },
         Rect { rectX=320, rectY=240, rectW=320, rectH=240 }] 
         :: RectArray

loop :: AppEnv ()
loop = do
    quit <- whileEvents $ modify . handleEvent'

    screen      <- screen `liftM` ask
    clips       <- clips `liftM` ask
    buttonSheet <- buttonSheet `liftM` ask
    button      <- get


    liftIO $ do
        bgColor  <- (mapRGB . surfaceGetPixelFormat) screen
            0xff 0xff 0xff
        clipRect <- Just `liftM` getClipRect screen
        fillRect screen clipRect bgColor
        showButton button clips buttonSheet screen
        SDL.flip screen

    unless quit loop
  where
    handleEvent' = Prelude.flip handleEvent

whileEvents :: MonadIO m => (Event -> m ()) -> m Bool
whileEvents act = do
    event <- liftIO pollEvent
    case event of
        Quit -> return True
        NoEvent -> return False
        _    -> do
            act event
            whileEvents act

main = withInit [InitEverything] $ do
        env <- initEnv
        runLoop env myButton
  where myButton = button 170 130 320 240
