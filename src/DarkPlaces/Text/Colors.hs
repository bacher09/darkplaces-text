module DarkPlaces.Text.Colors (
    RGB(..),
    getRGB,
    getColor,
    simplifyColor,
    hReset
) where
import Data.Bits
import System.Console.ANSI
import System.IO (Handle)

newtype RGB = RGB (Int, Int, Int) deriving(Show, Eq)
newtype HSV = HSV (Double, Double, Double) deriving(Show, Eq)


getRGB :: Int -> RGB
getRGB c = RGB (shiftR c 8 .&. 0xff, shiftR c 4 .&. 0xf , c .&. 0xf)


scaleRGB :: RGB -> Int -> RGB
scaleRGB (RGB (r, g, b)) s = RGB (r * s, g * s, b * s)


minMaxRGB :: (Num a, Ord a) => a -> a -> a -> (a, a)
minMaxRGB r g b = if r > g
            then if r > b
                then (if g < b then g else b, r)
                else (g, b)
            else if g > b
                then (if b < r then b else r, g)
                else (r, b)


rgbToHSV :: RGB -> HSV
rgbToHSV (RGB (r, g, b)) = HSV (hue, saturation, value)
  where
    (min, max) = minMaxRGB r g b
    delta = fromIntegral $ max - min :: Double
    hue
        | min == max = 0
        | r == max   = fromIntegral (60 * (g - b)) / delta
        | g == max   = fromIntegral (60 * (b - r)) / delta + 120
        | otherwise  = fromIntegral (60 * (r - g)) / delta + 240

    saturation = if max == 0 then 0 else 1 - fromIntegral min / fromIntegral max
    value = fromIntegral max / 255


-- from https://github.com/xonotic/darkplaces/blob/9973d76822ff8375e07694ea34c812fbbf8ebdbb/console.c#L1068
simplifyColor :: Int -> Int
simplifyColor hex_c
    | s < 0.2 && v < 0.5 = 0
    | s < 0.2 = 7
    | h < 36 = 1
    | h < 80 = 3
    | h < 150 = 2
    | h < 200 = 5
    | h < 270 = 4
    | h < 330 = 6
    | otherwise = 1
  where
    rgb = scaleRGB (getRGB hex_c) 17
    HSV (h, s, v) = rgbToHSV rgb


getColor :: Int -> [SGR]
getColor 1 = [SetConsoleIntensity BoldIntensity, SetColor Foreground Dull Red]
getColor 2 = [SetConsoleIntensity BoldIntensity, SetColor Foreground Dull Green]
getColor 3 = [SetConsoleIntensity BoldIntensity, SetColor Foreground Dull Yellow]
getColor 4 = [SetConsoleIntensity BoldIntensity, SetColor Foreground Dull Blue]
getColor 5 = [SetConsoleIntensity BoldIntensity, SetColor Foreground Dull Cyan]
getColor 6 = [SetConsoleIntensity BoldIntensity, SetColor Foreground Dull Magenta]
getColor n
    | n == 0 || n == 7 = [Reset]
    | n == 8 || n == 9 = [SetConsoleIntensity BoldIntensity, SetColor Foreground Dull  Black]
    | otherwise = []


hReset :: Handle -> IO ()
hReset h = hSetSGR h [Reset]
