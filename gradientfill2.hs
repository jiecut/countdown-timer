module GradientFill2 where

import Graphics.Win32

----------------------------------------------------------------

drawRect :: HDC -> INT -> INT -> INT -> INT -> COLORREF -> IO ()
drawRect hdc x0 y0 x1 y1 color = do
  hbrush <- createSolidBrush color
  fillRect hdc (x0, y0, x1, y1) hbrush
  deleteBrush hbrush

gradientFillRectH :: HDC -> (INT, INT, INT, INT) -> COLORREF -> COLORREF -> IO ()
gradientFillRectH hdc (x0, y0, x1, y1) color0 color1 = do
  let r0 = fromIntegral $ getRValue color0
  let g0 = fromIntegral $ getGValue color0
  let b0 = fromIntegral $ getBValue color0
  let r1 = fromIntegral $ getRValue color1
  let g1 = fromIntegral $ getGValue color1
  let b1 = fromIntegral $ getBValue color1
  flip mapM_ [y0 .. y1] $ \y -> do
    let p = fromIntegral (y - y0) / fromIntegral (y1 - y0)
    let q = 1 - p
    let r = floor $ p * r0 + q * r1
    let g = floor $ p * g0 + q * g1
    let b = floor $ p * b0 + q * b1
    drawRect hdc x0 y x1 (y+1) $ rgb r g b
