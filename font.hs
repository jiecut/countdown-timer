module Font where

import Graphics.Win32.GDI.Font
import Graphics.Win32.GDI.Types
import Foreign.Ptr (nullPtr)

-- :t createFont
-- createFont
--   :: System.Win32.Types.INT
--      -> System.Win32.Types.INT
--      -> System.Win32.Types.INT
--      -> System.Win32.Types.INT
--      -> FontWeight
--      -> Bool
--      -> Bool
--      -> Bool
--      -> CharSet
--      -> OutPrecision
--      -> ClipPrecision
--      -> FontQuality
--      -> PitchAndFamily
--      -> FaceName
--      -> IO HFONT

getFont :: Int -> IO HFONT
getFont height = createFont (fromIntegral height) 0 0 0 fW_DONTCARE False False False aNSI_CHARSET oUT_DEFAULT_PRECIS cLIP_DEFAULT_PRECIS dEFAULT_QUALITY (dEFAULT_PITCH + fF_DONTCARE) "Terminal"
