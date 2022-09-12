module Object where

import qualified Csg
import qualified Csg.STL
import qualified Data.Text.IO as T

screenWidth = 126

screenHeight = 101.0

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

batteryDepth = 44
batteryWidth = 22.5

batteryPadding = 5
batteryBevelR = 8

beveledSquare :: Double -> Double -> Double -> Csg.BspTree
beveledSquare r w d = 
  Csg.unionConcat $ 
    [Csg.scale (w, 1, d-r*2) Csg.unitCube, Csg.scale (w-r*2, 1, d) Csg.unitCube] <> 
      [Csg.translate ((w/2-r)*i, 0, (d/2-r)*j) $ Csg.scale (r,1,r) $ Csg.rotate (1, 0, 0) (pi/2) $ Csg.unitCylinder 16 | i <- [-1, 1], j <- [-1, 1]]

batteryCavity :: Csg.BspTree 
batteryCavity = let mainCav = Csg.scale (1, 1000, 1) $ beveledSquare batteryBevelR batteryWidth batteryDepth
                    crackWidth = 3
                    crack = (Csg.scale (crackWidth, 1000, 1000) $ Csg.translate (0, 0, -0.5) Csg.unitCube) `Csg.subtract`
                            (Csg.unionConcat [ Csg.translate (0,screenWidth/8*i, 0) $ Csg.scale (1000, crackWidth, 1000) $ Csg.unitCube | i <- [-1, 1]])
                    buttonScale = (10, 16, 1000)
                    buttonHole = Csg.translate (0, (-heightOuter/2) + 15, 0)$  Csg.scale buttonScale $ Csg.translate (0, 0, -0.5) Csg.unitCube
                    lightScale = (1000, 25, 10)
                    lightHole = Csg.translate (0, (-heightOuter/2) + 22, 0)$  Csg.scale lightScale $ Csg.translate (-0.5, 0, 0) Csg.unitCube
                in Csg.unionConcat [mainCav, crack, buttonHole, lightHole]

usbDepth = 18.5
usbWidth = 10
usbHeight = 24.5
usbR = 2.5 

usbCavity :: Csg.BspTree
usbCavity = Csg.scale (1, 1000, 1) $ Csg.translate (0, 0, 0) $ beveledSquare usbR usbWidth usbDepth

switchTracks :: Csg.BspTree
switchTracks = 
  let oneTrack = Csg.unionConcat [
                     Csg.scale (leftDepth + 7,10,4.5) $ Csg.translate (0.5, 0, -0.5) Csg.unitCube,
                     Csg.scale (leftDepth + 7,4,2) $ Csg.translate (0, 0, -0.5) Csg.unitCube,
                     Csg.translate (-1, 0, -1) $ Csg.scale (20, 6, 6) $ Csg.translate (-0.5, 0, 0.0) $ Csg.rotate(0, 1, 0) (pi/2) $ Csg.unitCylinder 8
                   ]
      trackD = 65/3
   in Csg.unionConcat [Csg.translate (0, (screenHeight/2)-(16 + (i*trackD)), 0) oneTrack | i <- [0..3]]

switch :: Csg.BspTree 
switch = let t = 0.5 
          in Csg.unionConcat [
               Csg.scale (3,10-t,4.5-t) $ Csg.translate (0.5, 0, -0.5) Csg.unitCube,
               Csg.scale (2,4-t,2-t) $ Csg.translate (-0.5, 0, -0.5) Csg.unitCube
             ]
stripHeight = 15
stripThickness = 4
stripDown = 13
stripMiddle = 32.5

topStripThickness = 6

topStrip :: Csg.BspTree
topStrip = (Csg.translate (0, screenHeight/2 - stripMiddle + 2 ,0) $ 
               ( (Csg.scale (107+borderLeft, stripHeight+7, topStripThickness) $ Csg.translate (0.5, -0.5, -0.5) Csg.unitCube) `Csg.subtract` 
                 (Csg.translate (108+borderLeft-35, 0, 0) $ Csg.scale (40, 20, 5) $ Csg.translate (0, 0,  0) Csg.unitCube)
               ) `Csg.union`
               (Csg.translate (0, -5, 0) $ Csg.scale (34+borderLeft,4, 40) $ Csg.translate (0.5, -0.5, -0.5) Csg.unitCube)
           ) 


bottomStrip :: Csg.BspTree
bottomStrip = Csg.translate (0, screenHeight/2 - stripMiddle,-stripDown)$ Csg.scale (90+borderWidthOuter, stripHeight, stripThickness) $ Csg.translate (-0.5, 0.5, -0.5) Csg.unitCube


bottomStripSupport :: Csg.BspTree
bottomStripSupport = let topR = 7 in Csg.unionConcat [ 
   --left
   Csg.translate (0, screenHeight/2 - stripMiddle,0)$ Csg.scale (18, stripHeight, usbDepth + 2 * 4-topR) $ Csg.translate (-0.5, 0.5, -0.5) Csg.unitCube,
   Csg.translate (-topR, screenHeight/2 - stripMiddle,0)$ Csg.scale (18-topR*2, stripHeight, usbDepth + 2 * 4) $ Csg.translate (-0.5, 0.5, -0.5) Csg.unitCube,
   let w = 18 
       d = usbDepth + 2 * 4
    in Csg.unionConcat [Csg.translate (((w/2-topR)*i)-w/2, screenHeight/2-stripMiddle+stripHeight/2, -d+topR) $ Csg.rotate (1, 0, 0) (pi/2) $ Csg.scale (topR, topR, stripHeight) $ Csg.unitCylinder 12 | i <- [-1, 1]],

   --top
   Csg.translate (-rightDepth, heightOuter/2 ,0)$ Csg.scale (90-rightDepth, stripHeight-stripThickness, stripThickness+stripDown) $ Csg.translate (0.5, -0.5, -0.5) Csg.unitCube,
   Csg.translate (-90, heightOuter/2 ,-borderWidthOuter)$ Csg.scale (20, stripHeight-stripThickness, stripThickness+stripDown-borderWidthOuter) $ Csg.translate (0.5, -0.5, -0.5) Csg.unitCube,
   Csg.translate (-90, screenHeight/2 ,-stripDown)$ Csg.scale (20, stripMiddle, stripThickness) $ Csg.translate (0.5, -0.5, -0.5) Csg.unitCube,
   --bottom

   Csg.translate (-65, -heightOuter/2 ,0)$ Csg.scale (20, stripHeight-stripThickness, stripThickness+stripDown) $ Csg.translate (0.5, 0.5, -0.5) Csg.unitCube,
   Csg.translate (-65, -screenHeight/2 ,-stripDown)$ Csg.scale (20, screenHeight - stripMiddle, stripThickness) $ Csg.translate (0.5, 0.5, -0.5) Csg.unitCube
   ]

piHoles :: Csg.BspTree
piHoles =
  Csg.translate (screenWidth/2 -27,screenHeight/2 - 21, 0) $  
   Csg.unionConcat [ Csg.translate ((65-7)*i,(30-7)*j,0) $ Csg.scale (1.5,1.5,100)$ Csg.unitCylinder 8 | i <- [0, -1], j <- [0,-1]]  


piNutHoles :: Csg.BspTree
piNutHoles =
  Csg.translate (screenWidth/2 -27,screenHeight/2 - 21, 0.01) $  
   Csg.unionConcat [ Csg.translate ((65-7)*i,(30-7)*j,0) $ Csg.scale (3.5,3.5,6)$ Csg.unitCylinder 6 | i <- [0, -1], j <- [-1]]  

cameraHolderThickness = 10
cameraThickness = 1.5
cameraWidth = 25
cameraHolderOuterW = cameraWidth + 10

cameraHolder :: Bool -> Csg.BspTree
cameraHolder b = let x = if b then 0.5 else -0.5
                     positive = Csg.translate (0, screenHeight/2 - stripMiddle,-stripDown) $
                                       Csg.scale (cameraHolderOuterW, stripHeight, cameraHolderThickness) $
                                         Csg.translate (0, x, -0.5) Csg.unitCube
                     negative1 = Csg.scale (cameraWidth - 2, 100,100) $ Csg.unitCube
                     negative2 = Csg.translate (0, 0,-stripDown-stripThickness-(cameraHolderThickness-stripThickness)*0.7) $
                                       Csg.scale (cameraWidth, 100, cameraThickness) $ Csg.unitCube
                  in Csg.translate (10, 0, 0) (positive `Csg.subtract` (negative1 `Csg.union` negative2))



batteryBlock = let 
      topR = 9
      d = batteryDepth + 2*batteryPadding
      w = batteryWidth + 2*batteryPadding
      bevel = Csg.unionConcat [Csg.translate (((w/2-topR)*i), 0, -d+topR) $ Csg.rotate (1, 0, 0) (pi/2) $ Csg.scale (topR, topR, heightOuter) $ Csg.unitCylinder 12 | i <- [-1, 1]]
      batteryBlockScale = (w, heightOuter,d-topR)
      batteryBlockScale' = (w-topR*2, heightOuter,d)
      in Csg.unionConcat [
        Csg.scale batteryBlockScale $ Csg.translate (0.5, 0, -0.5) Csg.unitCube,
        Csg.translate (topR, 0, 0) $ Csg.scale batteryBlockScale' $ Csg.translate (0.5, 0, -0.5) Csg.unitCube,
        Csg.translate (w/2, 0, 0)  bevel 
       ]

objectLeft :: Csg.BspTree
objectLeft = 
  let 
      left = -borderLeft - screenWidth/2
      frameScale = (leftDepth + borderLeft, heightOuter, screenT + 2*borderWidthOuter)
      frameBulk = Csg.translate (left, 0, -borderWidthOuter ) $ Csg.scale frameScale $ Csg.translate (0.5,0,0.5) Csg.unitCube 
      frameFragment = frameBulk `Csg.subtract` (screen `Csg.union` screenInner)
      batteryBlockPositioned = Csg.translate (left, 0, 0) batteryBlock
      batteryCavityPositioned = Csg.translate (left + batteryPadding + batteryWidth/2, 0, -batteryPadding - batteryDepth/2) $ batteryCavity
      switchTrackPositioned = Csg.translate (left + 2, 0, 0) switchTracks
      topStripPositioned = (Csg.translate (left, 0, 0) $ topStrip) `Csg.subtract` (Csg.union piHoles piNutHoles)
   in (frameFragment `Csg.union` batteryBlockPositioned  `Csg.union` topStripPositioned)
         `Csg.subtract`
        (batteryCavityPositioned `Csg.union` switchTrackPositioned )


objectRight :: Csg.BspTree
objectRight = 
  let 
      right = borderWidthOuter + screenWidth/2
      frameScale = (rightDepth + borderWidthOuter, heightOuter, screenT + 2*borderWidthOuter)
      frameBulk = Csg.translate (right, 0, -borderWidthOuter ) $ Csg.scale frameScale $ Csg.translate (-0.5,0,0.5) Csg.unitCube 
      frameFragment = frameBulk `Csg.subtract` (screen `Csg.union` screenInner)
      bottomStripPositioned = Csg.translate (right, 0, 0) bottomStrip
      bottomStripSupportPositioned = Csg.translate (right, 0, 0) bottomStripSupport
      usbCavityPositioned = Csg.translate (right- 4  - usbWidth/2, 0, -4 - usbDepth/2) $ usbCavity
   in (frameFragment `Csg.union` bottomStripPositioned `Csg.union` bottomStripSupportPositioned `Csg.union` cameraHolder True) 
        `Csg.subtract` ( piHoles `Csg.union` usbCavityPositioned)

pathLeft = "impression-case-left.stl"

pathRight = "impression-case-right.stl"
pathSwitch = "impression-case-switch.stl"
main :: IO ()
main = do 
  T.writeFile pathLeft $ Csg.STL.toSTL objectLeft
  T.writeFile pathRight $ Csg.STL.toSTL objectRight
  T.writeFile pathSwitch $ Csg.STL.toSTL switch
