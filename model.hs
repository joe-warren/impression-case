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
rightDepth = screenWidth - leftDepth

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
      [Csg.translate ((w/2-r)*i, 0, (d/2-r)*j) $ Csg.scale (r,1,r) $ Csg.rotate (1, 0, 0) (pi/2) $ Csg.unitCylinder 16 | i <- [-1, 1], j <- [-1, 1]]

batteryCavity :: Csg.BspTree 
batteryCavity = let mainCav = Csg.scale (1, 1000, 1) $ beveledSquare batteryBevelR batteryWidth batteryDepth
                    crackWidth = 3
                    crack = Csg.scale (crackWidth, 1000, 1000) $ Csg.translate (0, 0, -0.5) Csg.unitCube 
                    buttonScale = (10, 16, 1000)
                    buttonHole = Csg.translate (0, (-heightOuter/2) + 15, 0)$  Csg.scale buttonScale $ Csg.translate (0, 0, -0.5) Csg.unitCube
                    lightScale = (1000, 25, 10)
                    lightHole = Csg.translate (0, (-heightOuter/2) + 22, 0)$  Csg.scale lightScale $ Csg.translate (-0.5, 0, 0) Csg.unitCube
                in Csg.unionConcat [mainCav, crack, buttonHole, lightHole]

usbDepth = 18.5
usbWidth = 10
usbHeight = 25
usbR = 2 

usbCavity :: Csg.BspTree
usbCavity = Csg.scale (1, 1000, 1) $ beveledSquare usbR usbWidth usbDepth

switchTracks :: Csg.BspTree
switchTracks = 
  let oneTrack = Csg.unionConcat [
                     Csg.scale (100,10,4.5) $ Csg.translate (0.5, 0, -0.5) Csg.unitCube,
                     Csg.scale (200,4,2) $ Csg.translate (0, 0, -0.5) Csg.unitCube,
                     Csg.translate (-1, 0, -1) $ Csg.scale (20, 6, 6) $ Csg.translate (-0.5, 0, 0.0) $ Csg.rotate(0, 1, 0) (pi/2) $ Csg.unitCylinder 8
                   ]
      trackD = 65/3
   in Csg.unionConcat [Csg.translate (0, (screenHeight/2)-(16 + (i*trackD)), 0) oneTrack | i <- [0..3]]

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
      usbBlockScale = (usbWidth + 2*batteryPadding, usbHeight,usbDepth + 2*batteryPadding)
      usbBlock = Csg.translate (left+batteryPadding+batteryWidth, -heightOuter/2,0) $ Csg.scale usbBlockScale $ Csg.translate (0.5, 0.5, -0.5) Csg.unitCube
      usbCavityPositioned = Csg.translate (left + batteryPadding*2 + batteryWidth + usbWidth/2, 0, -batteryPadding - usbDepth/2) $ usbCavity
      switchTrackPositioned = Csg.translate (left + 2, 0, 0) switchTracks
   in (frameFragment `Csg.union` batteryBlock `Csg.union` usbBlock)
         `Csg.subtract`
        (batteryCavityPositioned `Csg.union` switchTrackPositioned `Csg.union` usbCavityPositioned)


objectRight :: Csg.BspTree
objectRight = 
  let 
      right = borderWidthOuter + screenWidth/2
      frameScale = (rightDepth + borderWidthOuter, heightOuter, screenT + 2*borderWidthOuter)
      frameBulk = Csg.translate (right, 0, -borderWidthOuter ) $ Csg.scale frameScale $ Csg.translate (-0.5,0,0.5) Csg.unitCube 
      frameFragment = frameBulk `Csg.subtract` (screen `Csg.union` screenInner)
   in (frameFragment)

pathLeft = "impression-case-left.stl"

pathRight = "impression-case-right.stl"
main :: IO ()
main = do 
  T.writeFile pathLeft $ Csg.STL.toSTL objectLeft
  T.writeFile pathRight $ Csg.STL.toSTL objectRight
