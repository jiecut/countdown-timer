-- Compile with the option `-optl-mwindows' to hide the command prompt
-- Win32全般についてはここがわかりやすい http://wisdom.sakura.ne.jp/system/winapi/win32/index.html

{-
Usage:
Enter     - starts counting down
Esc       - stop counting down
Left      - adds 1 minute
Right     - subtracts 1 minute
Up        - adds 10 minutes
Down      - subtracts 10 minutes
Page Up   - adds 1 hour
Page Down - subtracts 1 hour
-}

{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Control.Monad
import Data.IORef
import System.Locale (defaultTimeLocale)
import Data.Time
import Data.Bits (shift)
import Data.Int (Int16)
import System.IO
import System.IO.Error (catchIOError)
import Data.List.Split (splitOn)

import Graphics.Win32
import System.Win32.Types
import System.Win32.DLL (getModuleHandle)

import TheTime
import Font
import Sound
-- import GradientFill
import GradientFill2

foreign import ccall "PostQuitMessage" postQuitMessage :: Int -> IO ()

----------------------------------------------------------------    

createMyWindow :: LPPAINTSTRUCT -> IO HWND
createMyWindow lpps = do
  hinst <- getModuleHandle Nothing
  window <- makeWindowProc lpps
  createWindowEx
    0x02000000 {- WS_EX_COMPOSITED -} -- extended window style
    className -- class name
    "Countdown Timer" -- window's title
    (wS_CAPTION + wS_MINIMIZEBOX + wS_SYSMENU) -- window style -- See https://msdn.microsoft.com/library/czada357%28v=vs.110%29.aspx
    Nothing Nothing Nothing Nothing -- position
    Nothing -- parent
    Nothing -- hmenu
    hinst -- hinstance
    --(makeWindowProc var lpps)
    window

----------------------------------------------------------------    

className :: LPCTSTR
className = mkClassName "CountdownTimer"

registerMyClass :: IO (Maybe ATOM)
registerMyClass = do
   hinst <- getModuleHandle Nothing
   hcursor <- loadCursor Nothing iDC_ARROW
   whiteBrush <- getStockBrush wHITE_BRUSH
   -- WNDCLASSがよくわかんないけどタプルを渡せばいいみたい
   registerClass (cS_HREDRAW + cS_VREDRAW, -- + cS_OWNDC, -- cS_HREDRAW + cS_VREDRAW, -- style
                  hinst, -- hintstance
                  Nothing, -- hicon
                  Just hcursor, -- hcursor
                  Just whiteBrush, -- hbrush
                  Nothing, -- menu name
                  className) -- class name

unregisterMyClass :: IO ()
unregisterMyClass = do
  hinst <- getModuleHandle Nothing
  unregisterClass className hinst

----------------------------------------------------------------

pump :: LPMSG -> IO () -- LPMSGはメッセージへのポインタ
pump lpmsg = do
  fContinue <- getMessage lpmsg Nothing
  when fContinue $ do
    translateMessage lpmsg
    dispatchMessage lpmsg
    pump lpmsg

----------------------------------------------------------------

dummyDay :: Day
dummyDay = fromGregorian 2000 1 1

showDiffTime :: DiffTime -> String
showDiffTime diff = formatTime defaultTimeLocale "%H:%M:%S" $ UTCTime dummyDay diff

showZonedTime :: UTCTime -> IO String
showZonedTime time = do
  zonedTime <- utcToLocalZonedTime time
  return $ formatTime defaultTimeLocale "%H:%M:%S" zonedTime

----------------------------------------------------------------

-- 余白を作って見た目をよくする。

phi :: Float
phi = (1 + sqrt 5) / 2

calcSize :: (Int, Int) -> (Int, Int)
calcSize (w, h) = (round $ (fromIntegral w) * (1 + phi) / phi, round $ (fromIntegral h) * (1 + phi) / phi)

calcStringPosition :: (Int, Int) -> (Int, Int)
calcStringPosition (w, h) = (round $ (fromIntegral w) / 2 / phi, round $ (fromIntegral h) / 2 / phi)

----------------------------------------------------------------

getBorderSize :: HWND -> IO (Int, Int)
getBorderSize hwnd = do
  (x0, y0, x1, y1) <- getWindowRect hwnd
  (_, _, w, h) <- getClientRect hwnd
  let ww = fromIntegral $ (x1 - x0) - w
  let hh = fromIntegral $ (y1 - y0) - h
  return (ww, hh)

getClockStringSize :: HDC -> IO (Int, Int)
getClockStringSize hdc = do
  (w, h) <- getTextExtentPoint32 hdc "00:00:00"
  return (fromIntegral w, fromIntegral h)

resizeWindow :: HWND -> HDC -> IO ()
resizeWindow hwnd hdc = do
  (x, y, _, _) <- getWindowRect hwnd
  (w', h') <- getBorderSize hwnd
  (w, h) <- getClockStringSize hdc
  let (ww, hh) = calcSize (w, h)
  moveWindow hwnd (fromIntegral x) (fromIntegral y) (w' + ww) (h' + hh) True

----------------------------------------------------------------

fillBackground :: HWND -> HDC -> COLORREF -> IO ()
fillBackground hwnd hdc color = do
  hbrush <- createSolidBrush color
  (_, _, w, h) <- getClientRect hwnd
  fillRect hdc (0,0,w,h) hbrush
  deleteBrush hbrush

----------------------------------------------------------------

toInt16 x = (fromIntegral x) :: Int16

----------------------------------------------------------------

saveConfig :: (Integer, Integer, (Int, Int)) -> IO ()
saveConfig (timeValue, fontHeight, windowLocation) = do
  handle <- openFile configFilePath WriteMode
  hPutStr handle $ show timeValue ; hPutStrLn handle " -- timeValue"
  hPutStr handle $ show fontHeight ; hPutStrLn handle " -- fontHeight"
  hPutStr handle $ show windowLocation ; hPutStrLn handle " -- windowLocation"
  hClose handle

removeComment :: String -> String
removeComment string = head $ splitOn "--" string
  
loadConfig :: (Integer, Integer, (Int, Int)) -> IO (Integer, Integer, (Int, Int))
loadConfig config =
  flip catchIOError (\_ -> return config) $ do
    lazyString <- readFile configFilePath
    let timeValue:fontHeight:windowLocation:_ = map removeComment $ lines lazyString
    return (read timeValue, read fontHeight, read windowLocation)

----------------------------------------------------------------

bounds :: Integer -> Integer
bounds timeValue = if timeValue >= 24*60*60 then 24*60*60 - 1 else if timeValue < 0 then 0 else timeValue

makeWindowProc :: LPPAINTSTRUCT -> IO (HWND -> WindowMessage -> WPARAM -> LPARAM -> IO LRESULT)
makeWindowProc lpps = do
  (timeValue, fontHeight, windowLocation) <- loadConfig defaultConfig
  varTimeValue <- newIORef timeValue
  varTheTime <- newIORef (0,0,-1)
  varFontHeight <- newIORef fontHeight
  varSavedDC <- newIORef 0
  varAppState <- newIORef Idle
  return $ windowProc varTimeValue varTheTime varFontHeight varSavedDC varAppState windowLocation lpps

initDC :: HDC -> IORef Integer -> IO ()
initDC hdc varFontHeight = do          
  setBkMode hdc tRANSPARENT
  setTextColor hdc $ colorTheme !! 0 -- rgb 174 255 249 -- 255 0 0
  fontHeight <- readIORef varFontHeight
  hfont <- getFont $ fromIntegral fontHeight
  selectFont hdc hfont
  return ()

windowProc varTimeValue varTheTime varFontHeight varSavedDC varAppState windowLocation lpps hwnd wm wp lp
  | wm == wM_DESTROY = do
      timeValue <- readIORef varTimeValue
      fontHeight <- readIORef varFontHeight
      (x, y, _, _) <- getWindowRect hwnd
      saveConfig (timeValue, fontHeight, (fromIntegral x, fromIntegral y))
      postQuitMessage 0
      return 0
  | wm == wM_USER = do -- なぜかwM_CREATEが来ないので、自分で送ったのを受け取る
      ----------------
      hdc <- getDC $ Just hwnd
      ----------------
      initDC hdc varFontHeight
      let (x, y) = windowLocation
      moveWindow hwnd (fromIntegral x) (fromIntegral y) 42 42 False
      resizeWindow hwnd hdc
      ----------------
      releaseDC (Just hwnd) hdc
      ----------------
      return 0
  | wm == wM_TIMER = do
      -- 表示されてる文字がチラつかないように必要ないときは何もしない
      pre <- readIORef varTheTime
      now <- getTheTime 0
      if pre == now
        then return 0 -- do nothing
        else do
        appState <- readIORef varAppState
        current <- getCurrentTime
        case appState of
          Idle -> do
            string <- showZonedTime current
            setWindowText hwnd string
          CountingDown target -> do
            if target <= current
              then do
              playAsyncLoop "alarm.wav"
              showWindow hwnd sW_RESTORE
              writeIORef varAppState $ Alarming 3
              else
              return ()
            invalidateRect (Just hwnd) Nothing False
          Alarming _ -> do
            invalidateRect (Just hwnd) Nothing False
            return () -- do nothing
        writeIORef varTheTime now
        return 0
  | wm == wM_KEYDOWN = do
      appState <- readIORef varAppState
      case appState of
        Idle ->
          let ff i = do
                modifyIORef varTimeValue (+ i)
                modifyIORef varTimeValue bounds
                invalidateRect (Just hwnd) Nothing False in
          case () of _
                       | wp == vK_ESCAPE -> do
                           sendMessage hwnd wM_DESTROY 0 0
                       | wp == vK_LEFT -> do -- See https://downloads.haskell.org/~ghc/7.8.1/docs/html/libraries/Win32-2.3.0.2/Graphics-Win32-Key.html
                           ff 60
                           return 0
                       | wp == vK_RIGHT -> do
                           ff (-60)
                           return 0
                       | wp == vK_UP -> do
                           ff 600
                           return 0
                       | wp == vK_DOWN -> do
                           ff (-600)
                           return 0
                       | wp == 33 {- Page Up -} -> do -- 定義されてないみたい
                           ff 3600
                           return 0
                       | wp == 34 {- Page Down -} -> do
                           ff (-3600)
                           return 0
                       | wp == vK_BACK || wp == vK_DELETE -> do
                           writeIORef varTimeValue 0
                           invalidateRect (Just hwnd) Nothing True -- False
                           return 0
                       | wp == vK_RETURN -> do
                           timeValue <- readIORef varTimeValue
                           current <- getCurrentTime
                           appState <- readIORef varAppState
                           if timeValue == 0
                             then return 0 -- do nothing
                             else
                             case appState of
                               Idle -> do
                                 let newTarget = addUTCTime (fromInteger timeValue :: NominalDiffTime) current
                                 string <- showZonedTime newTarget
                                 setWindowText hwnd $ "Counting down to " ++ string
                                 showWindow hwnd sW_MINIMIZE
                                 writeIORef varAppState $ CountingDown newTarget
                                 return 0
                               _ -> return 0
                       | otherwise -> do
                           -- putStr "Key is being pressed down: " >> print wp
                           return 0
        CountingDown _ ->
          if wp == vK_ESCAPE
          then do
            writeIORef varAppState Idle
            current <- getCurrentTime
            string <- showZonedTime current
            setWindowText hwnd string
            invalidateRect (Just hwnd) Nothing False
            return 0
          else return 0
        Alarming _ -> do
          writeIORef varAppState Idle
          stopPlayingSound
          invalidateRect (Just hwnd) Nothing False
          return 0
  | wm == 522 {- MOUSEWHEEL -} = do
      let delta = (*) (-1) $ toInteger $ flip quot 120 $ toInt16 $ shift wp (-16)
      fontHeight <- readIORef varFontHeight
      let newHeight = fontHeight + delta
      writeIORef varFontHeight newHeight
      ----------------
      hdc <- getDC $ Just hwnd
      ----------------
      initDC hdc varFontHeight
      hfont <- getCurrentFont hdc
      newHfont <- getFont (fromIntegral newHeight)
      selectFont hdc newHfont
      deleteBrush hfont
      resizeWindow hwnd hdc
      ----------------
      releaseDC (Just hwnd) hdc
      ----------------
      return 0
  | wm == wM_ERASEBKGND =
      return 0 -- do nothing
  | wm == wM_PAINT = do
      ----------------
      hdc <- beginPaint hwnd lpps
      ----------------
      initDC hdc varFontHeight
      rect <- getClientRect hwnd
      gradientFillRectH hdc rect (colorTheme !! 1) (colorTheme !! 2)
      appState <- readIORef varAppState
      case appState of
        Idle -> do
          timeValue <- readIORef varTimeValue
          (w, h) <- getClockStringSize hdc
          let (x, y) = calcStringPosition (w, h)
          textOut hdc (fromIntegral x) (fromIntegral y) $ showDiffTime $ secondsToDiffTime timeValue -- $ toInteger timeValue -- (timeValue :: DiffTime) -- formatTime defaultTimeLocale "%H:%M:%S" now
        CountingDown target -> do
          current <- getCurrentTime
          (w, h) <- getClockStringSize hdc
          let (x, y) = calcStringPosition (w, h)
          textOut hdc (fromIntegral x) (fromIntegral y) $ showDiffTime $ secondsToDiffTime $ round $ diffUTCTime target current
        Alarming numberOfTimes -> do
          (_, _, s) <- getTheTime 0
          if even numberOfTimes
            then do
            (w, h) <- getClockStringSize hdc
            let (x, y) = calcStringPosition (w, h)
	    textOut hdc (fromIntegral x) (fromIntegral y) "00:00:00"
            else do
            fillBackground hwnd hdc $ colorTheme !! 0
            setTextColor hdc $ colorTheme !! 1
            (w, h) <- getClockStringSize hdc
            let (x, y) = calcStringPosition (w, h)
	    textOut hdc (fromIntegral x) (fromIntegral y) "00:00:00"
          writeIORef varAppState $ Alarming $ (\n -> if n < 0 then 0 else n) $ (subtract 1) numberOfTimes
	  return ()
      ----------------
      endPaint hwnd lpps
      ----------------
      return 0
  | otherwise =
      defWindowProc (Just hwnd) wm wp lp

----------------------------------------------------------------

data AppState = Idle | CountingDown UTCTime | Alarming Int

configFilePath :: String
configFilePath = "config"

defaultConfig :: (Integer, Integer, (Int, Int))
defaultConfig = (3 * 60, 80, (250, 250)) -- timeValue, fontHeight, windowLocation

colorTheme :: [COLORREF]
-- colorTheme = [rgb 233 218 213, rgb 25 71 110, rgb 65 192 254]
-- colorTheme = [rgb 255 255 255, rgb 25 71 110, rgb 65 192 254]
-- colorTheme = [rgb 2 184 197, rgb 233 222 226, rgb 201 171 161]
-- colorTheme = [rgb 255 255 255, rgb 241 129 1, rgb 241 129 1] -- rgb 230 120 1] -- rgb 254 150 0] -- rgb 254 213 0]
colorTheme = [rgb 255 255 255, rgb 255 145 15, rgb 241 129 1]

main :: IO ()
main = 
  allocaPAINTSTRUCT $ \lpps -> do -- 描画するたびにmallocを使ってPAINTSTRUCTを確保するのが標準的。だけど基本的に一回確保すればいいはずのものなので、そうする。
  registerMyClass
  hwnd <- createMyWindow lpps
  sendMessage hwnd wM_USER 0 0 -- wM_Createの代わり
  showWindow hwnd sW_SHOWNORMAL -- sW_SHOW -- sW_SHOWNORMAL
  setWinTimer hwnd 42 100 {- ms -} -- wM_TIMERが0.1秒ごとに送られるようになる -- 250 {- ms -} -- wM_TIMERが0.25秒ごとに送られるようになる
  allocaMessage pump
  unregisterMyClass

 
