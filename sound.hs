{-# LANGUAGE ForeignFunctionInterface #-}

module Sound where

import Foreign
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.C.String
import System.Win32

-- メモ： LPCSTR = Ptr CChar

foreign import ccall "PlaySound" playSound :: Ptr a -> INT -> INT -> IO Bool

playAsyncLoop :: String -> IO Bool
playAsyncLoop wavFile = withCAString wavFile $ \f -> playSound f 0 $ 0x1 {- SND_ASYNC -} + 0x8 {- SND_LOOP -} + 0x20000 {- SND_FILENAME -}

stopPlayingSound :: IO Bool
stopPlayingSound = playSound nullPtr 0 0
