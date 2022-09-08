module Object where

import qualified Csg
import qualified Csg.STL
import qualified Data.Text.IO as T

screenWidth = 131.5

screenHeight = 102.0

screenT = 4.0

borderWidthInner = 2
borderWidthOuter = 2
borderLeft = 5

leftDepth = 45

heightOuter = screenHeight + 2*borderWidthOuter

screen :: Csg.BspTree
screen = Csg.scale (screenWidth, screenHeight, screenT) $Csg.translate (0, 0, 0.5) $ Csg.unitCube

screenInner :: Csg.BspTree
screenInner = Csg.scale (screenWidth-borderWidthInner*2, screenHeight-borderWidthInner*2, 100) $Csg.translate (0, 0, 0.0) $ Csg.unitCube

batteryDepth = 55
batteryWidth = 23

batteryPadding = 5

batteryBevelR = 7

beveledSquare :: Double -> Double -> Double -> Csg.BspTree
beveledSquare r w d = 
  Csg.unionConcat $ 
    [Csg.scale (w, 1, d-r*2) Csg.unitCube, Csg.scale (w-r*2, 1, d) Csg.unitCube] <> 
      [Csg.translate ((w/2-r)*i, 0, (d/2-r)*j) $ Csg.scale (r,1,r) $ Csg.rotate (1, 0, 0) (pi/2) $ Csg.unitCylinder 32 | i <- [-1, 1], j <- [-1, 1]]

batteryCavity :: Csg.BspTree 
batteryCavity = let mainCav = Csg.scale (1, 1000, 1) $ beveledSquare batteryBevelR batteryWidth batteryDepth
                    crackWidth = 3
                    crack = Csg.scale (crackWidth, 1000, 1000) $ Csg.translate (0, 0, -0.5) Csg.unitCube 
                    buttonScale = (10, 16, 1000)
                    buttonHole = Csg.translate (0, (-heightOuter/2) + 15, 0)$  Csg.scale buttonScale $ Csg.translate (0, 0, -0.5) Csg.unitCube
                    lightScale = (1000, 25, 10)
                    lightHole = Csg.translate (0, (-heightOuter/2) + 22, 0)$  Csg.scale lightScale $ Csg.translate (-0.5, 0, 0) Csg.unitCube
                in Csg.unionConcat [mainCav, crack, buttonHole, lightHole]

objectLeft :: Csg.BspTree
objectLeft = 
  let 
      left = -borderLeft - screenWidth/2
      frameScale = (leftDepth + borderLeft, heightOuter, screenT + 2*borderWidthOuter)
      frameBulk = Csg.translate (left, 0, -borderWidthOuter ) $ Csg.scale frameScale $ Csg.translate (0.5,0,0.5) Csg.unitCube 
      frameFragment = frameBulk `Csg.subtract` (screen `Csg.union` screenInner)
      batteryBlockScale = (batteryWidth + 2*batteryPadding, heightOuter,batteryDepth + 2*batteryPadding)
      batteryBlock = Csg.translate (left, 0,0) $ Csg.scale batteryBlockScale $ Csg.translate (0.5, 0, -0.5) Csg.unitCube
      batteryCavityPositioned = Csg.translate (left + batteryPadding + batteryWidth/2, 0, -batteryPadding - batteryDepth/2) $ batteryCavity
   in frameFragment `Csg.union` (batteryBlock `Csg.subtract` batteryCavityPositioned)

pathLeft = "impression-case-left.stl"

main :: IO ()
main = do 
  T.writeFile pathLeft $ Csg.STL.toSTL objectLeft
