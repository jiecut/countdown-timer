-- GradientFillについて http://www.flounder.com/gradientfiller.htm
-- これを使っても残念ながら真っ黒く描画されるだけ。なのでgradientfill2を使う。

{-# LANGUAGE ForeignFunctionInterface #-}

module GradientFill where

import Foreign
import Foreign.Ptr (Ptr, plusPtr)
import System.Win32
import Graphics.Win32.GDI.Types (HDC)
import Control.Monad (zipWithM_)

#include <windows.h>
#include <WinGdi.h>

----------------------------------------------------------------

type TRIVERTEX =
  ( LONG -- x
  , LONG -- y
  , INT -- r
  , INT -- g
  , INT -- b
  , INT -- a
  )

sizeofTRIVERTEX :: Int
sizeofTRIVERTEX = #{size TRIVERTEX}                   
  
withTRIVERTEXArray :: [TRIVERTEX] -> (Ptr TRIVERTEX -> Int -> IO a) -> IO a
withTRIVERTEXArray xs f =
  allocaBytes (sizeofTRIVERTEX * len) $ \ptr -> do
  pokeTRIVERTEXArray ptr xs
  f ptr len
  where
    len = length xs

pokeTRIVERTEXArray :: Ptr TRIVERTEX -> [TRIVERTEX] -> IO ()
pokeTRIVERTEXArray ptr xs =
  zipWithM_ (setTRIVERTEX ptr) [0..] xs

setTRIVERTEX :: Ptr TRIVERTEX -> Int -> TRIVERTEX -> IO ()
setTRIVERTEX ptr off = pokeTRIVERTEX $ plusPtr ptr $ off * sizeofTRIVERTEX

pokeTRIVERTEX :: Ptr TRIVERTEX -> TRIVERTEX -> IO ()
pokeTRIVERTEX p (x, y, r, b, g, a) = do
  #{poke TRIVERTEX, x} p x
  #{poke TRIVERTEX, y} p y
  #{poke TRIVERTEX, Red} p r
  #{poke TRIVERTEX, Green} p g
  #{poke TRIVERTEX, Blue} p b
  #{poke TRIVERTEX, Alpha} p a

----------------------------------------------------------------

-- DWORD = ULONG

type GRADIENT_RECT =
  ( DWORD -- x
  , DWORD -- y
  )

sizeofGRADIENT_RECT :: Int
sizeofGRADIENT_RECT = #{size GRADIENT_RECT}                   

withGRADIENT_RECTArray :: [GRADIENT_RECT] -> (Ptr GRADIENT_RECT -> Int -> IO a) -> IO a
withGRADIENT_RECTArray xs f =
  allocaBytes (sizeofGRADIENT_RECT * len) $ \ptr -> do
  pokeGRADIENT_RECTArray ptr xs
  f ptr len
  where
    len = length xs

pokeGRADIENT_RECTArray :: Ptr GRADIENT_RECT -> [GRADIENT_RECT] -> IO ()
pokeGRADIENT_RECTArray ptr xs =
  zipWithM_ (setGRADIENT_RECT ptr) [0..] xs

setGRADIENT_RECT :: Ptr GRADIENT_RECT -> Int -> GRADIENT_RECT -> IO ()
setGRADIENT_RECT ptr off = pokeGRADIENT_RECT $ plusPtr ptr $ off * sizeofGRADIENT_RECT

pokeGRADIENT_RECT :: Ptr GRADIENT_RECT -> GRADIENT_RECT -> IO ()
pokeGRADIENT_RECT p (upperLeft, lowerRight) = do
  #{poke GRADIENT_RECT, UpperLeft} p upperLeft
  #{poke GRADIENT_RECT, LowerRight} p lowerRight

----------------------------------------------------------------

gradientFillRectH :: HDC -> [TRIVERTEX] -> [GRADIENT_RECT] -> IO Bool
gradientFillRectH hdc verts meshes =
  withTRIVERTEXArray verts $ \vertArray numVerts -> 
  withGRADIENT_RECTArray meshes $ \meshArray numMeshes -> do
  print verts
  print meshes
  print vertArray
  print numVerts
  print meshArray
  print numMeshes
  c_gradientFill hdc vertArray (fromIntegral numVerts) meshArray (fromIntegral numMeshes) 0 {- GRADIENT_FILL_RECT_H -}

gradientFoo hdc = gradientFillRectH hdc [(210,10,255,255,255,255), (300,100,0,0,0,255)] [(0, 1)]
gradientBar hdc = gradientFillRectH hdc [(410,10,255,255,255,0), (500,100,0,0,0,0)] [(0, 1)]

foreign import ccall "GdiGradientFill" c_gradientFill :: HDC -> Ptr TRIVERTEX -> DWORD -> Ptr GRADIENT_RECT -> DWORD -> DWORD -> IO Bool
