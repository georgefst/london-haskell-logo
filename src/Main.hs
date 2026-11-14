{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main (main) where

import Codec.Picture
import Data.Bifunctor
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSL (hsl)
import Data.Colour.RGBSpace.HSV
import Data.Complex
import Data.Fixed
import Data.List (find, minimumBy)
import Data.Ord
import Data.Word
import Diagrams.Prelude hiding (size, outer, font, lerp)
import Diagrams.Prelude qualified as D
import Diagrams.TwoD.Image (embeddedImage)
import DiagramsWaterfall
import Linear.V3
import Waterfall qualified as W

bound = 16
maxIterations = 50
power = 2
iterationsToColour inner outer = \case
    Nothing -> inner
    Just n ->
        let t = n / fromIntegral maxIterations
         in hsvBlend (t ** e) outer inner
  where
    e = 1.3

smooth n z = max 0 $ fromIntegral n - log (log (magnitude z) / log bound) / log power

divergenceIterations c =
    fmap (second (- c))
        . find ((> (bound ^ 2)) . magnitudeSquared . snd)
        . zip [0 :: Int ..]
        . take maxIterations
        $ iterate (\z -> z ** (power :+ 0) + c) c
  where
    magnitudeSquared (x :+ y) = x * x + y * y

mandelbrot w h (centreX, centreY) size innerColour outerColour =
    generateImage
        ( curry $
            convertColour
                . toSRGB
                . iterationsToColour innerColour outerColour
                . fmap (uncurry smooth)
                . divergenceIterations
                . pixelToComplex
        )
        w
        h
  where
    (xMin, xMax) = (centreX - size / 2, centreX + size / 2)
    (yMin, yMax) = (centreY - size / 2, centreY + size / 2)
    pixelToComplex (x, y) =
        (fromIntegral x / fromIntegral w * (xMax - xMin) + xMin)
            :+ (fromIntegral y / fromIntegral h * (yMin - yMax) + yMax)
    convertColour (RGB r g b) = PixelRGB16 (floor $ m * r) (floor $ m * g) (floor $ m * b)
      where
        m = fromIntegral $ maxBound @Word16

hsvBlend t c1 c2 = uncurryRGB sRGB $ hsv (lerpWrap 360 h1 h2) (lerp s1 s2) (lerp v1 v2)
  where
    (h1, s1, v1) = hsvView $ toSRGB c1
    (h2, s2, v2) = hsvView $ toSRGB c2
    lerp a b = (1 - t) * a + t * b
    lerpWrap m a b = lerp a (minimumBy (comparing (abs . subtract a)) [b - m, b, b + m]) `mod'` m

stationCircle = joinedStations . pure

joinedStations :: [Point V2 Double] -> Diagram B
joinedStations points = 
    let trail = fromVertices points
        points3D = map (\(P (V2 x y)) -> P (V3 x y 0)) points
    in  atPoints points3D (repeat (sphere # skin # scale 2 # sc white # translateZ 1))
            <> mconcat (zipWith
                (\a b -> let v = b .-. a in cylinder # skin # scaleZ (norm v)
                    # transform (rotationAbout 0 (direction $ cross unitZ v) (angleBetween unitZ v))
                    # moveTo a # sc white # translateZ 1.5
                ) points3D (drop 1 points3D))
            <> atPoints points (repeat (circle 3 # fc black)) # extrude 2
            <> trail # lc black # lwG 3 # extrude 2

circleLine = 
    lc (sRGB24 255 211 0)

victoriaLine = 
    lc (sRGB24 0 152 212)

bakerlooLine =
    lc (sRGB24 179 99 5)

centralLine =
    lc (sRGB24 227 32 23)

hammersmithAndCityLine = 
    lc (sRGB24 243 169 187)

districtLine = 
    lc (sRGB24 0 120 42)

leg = fromSegments 
    [straight (r2 (-10, 10))
    , bezier3 zero (r2 (-2, 2)) (r2 (-2, 4))
    , straight (r2 (0, 4))
    ]

horizontalA = straight (r2 (24, 0)) `at` (p2 (6, 6))

horizontalB = straight (r2 (12, 0)) `at` (p2 (18, -6))

upperLine = translate (r2 (0, 1))
lowerLine = translate (r2 (0, -1))

lambdaTube =
    ( mconcat
    [ joinedStations [zero, p2 (6, 6)]
    , stationCircle $ p2 (12, 0)
    , stationCircle $ p2 (18, (-6))
    ] <> mconcat
    [ leg # lwG 2 # victoriaLine
    , leg # reflectY # lwG 2 # victoriaLine
    , leg # translate (12 *^ unit _x) # lwG 2 # bakerlooLine
    , leg # reflectY # reflectX # translate (12 *^ unit _x) # lwG 2 # bakerlooLine
    , leg # reflectY # translate (12 *^ unit _x) # lwG 2 # centralLine
    , horizontalA # stroke # lwG 2 # hammersmithAndCityLine # upperLine 
    , horizontalA # stroke # lwG 2 # circleLine # lowerLine
    , horizontalB # stroke # lwG 2 # districtLine # lowerLine 
    , horizontalB # stroke # lwG 2 # circleLine # upperLine
    ]
        # extrude 2
    )
    # center

logoName = 
    text "London Haskell"
        # D.font "P22 Johnston Underground"
        # fontSizeG 8
        # fc (sRGB24 0 54 136)

diag =
    (atPoints [0, p3 (0, -14, 1)] [lambdaTube, logoName # extrude 0.5])
    # pad 1.5

mandelbrotCube =
    image
        ( embeddedImage $
            ImageRGB16 $
                mandelbrot
                    1024
                    1024
                    (-0.6, 0)
                    3
                    (uncurryRGB sRGB $ hsl 213 0.76 (if inverted then 0.02 else 0.55))
                    (uncurryRGB sRGB $ hsl 213 0.76 (if inverted then 0.55 else 0.02))
        )
        # scaleUToX 10
        # extrude 10
  where
    inverted = False

main = do
    -- renderSVG "logo.svg" (mkSizeSpec $ V2 Nothing (Just 72)) diag
    globalFont <- W.fontFromPath "P22 Johnston Underground Regular.ttf" 8
    W.writeSolid 0.1 "mandelbrot-cube.glb" $ renderWaterfall globalFont mandelbrotCube
    W.writeSolid 0.1 "logo.glb" $
        renderWaterfall
            globalFont
            -- $ mconcat
            --     [ cube & skin & scale 10 & sc blue & translateX 5
            --     , cube & skin & scale 10 & sc red & translateZ -20 & translateX 5
            --     , sphere & skin & translateY 1 & scale 5 & sc green & scaleZ 3 & translateX -5
            --     ]
            --     # translateX -40
            --     # translateZ -20
            -- <>
            diag
