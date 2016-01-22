import Data.List
import Text.Regex
import System.Random
import Data.Ord

type Point     = (Float,Float)
type Color     = (Int,Int,Int)
type Polygon   = [Point]
type Person    = [Int]
type Link      = [Point]
type Placement = [(Point,Person)]

type EnergyFunction a              = a -> Int
type TemperatureFunction           = Int -> Int -> Float
type TransitionProbabilityFunction = Int -> Int -> Float -> Float
type MotionFunction a              = StdGen -> a -> (StdGen,a)

main = do 
  putStr "Hello World! Let's have a picnic! \n"
  people_text <- readFile "people.txt"

  let people :: [Person]
      people = read people_text

  putStr "Number of people coming: "
  print (length people)
  
  let writePoint :: Point -> String 
      writePoint (x,y) = (show x)++","++(show y)++" "

  let writePolygon :: (Color,Polygon) -> String 
      writePolygon ((r,g,b),p) = "<polygon points=\""++
                (concatMap writePoint p)++ "\" style=\"fill:#cccccc;stroke:rgb("++
                (show r)++","++(show g)++","++(show b)++");stroke-width:2\"/>"

  let writePolygons :: [(Color,Polygon)] -> String 
      writePolygons p = "<svg xmlns=\"http://www.w3.org/2000/svg\">"++
                        (concatMap writePolygon p)++"</svg>"

  let colorize :: Color -> [Polygon] -> [(Color,Polygon)] 
      colorize = zip.repeat

  let rainbow@[red,green,blue,yellow,purple,teal] = map colorize [
       (255,0,0),(0,255,0),(0,0,255),(255,255,0),(255,0,255),(0,255,255)]

  --print $ (rainbow!!2) [[(100,100),(200,100),(200,200),(100,200)],[(200,200),(300,200),(300,300),(200,300)]]

  --writeFile "tut0.svg" $ writePolygons (blue [

  let readPoint :: String -> Point
      readPoint s | Just [x,y] <- matchRegex (mkRegex "([0-9.]+),([0-9.]+)") s = (read x, read y)

  let readPolygon :: String -> Polygon
      readPolygon = (map readPoint).(splitRegex $ mkRegex " L ")

  let readPolygons :: String -> [Polygon]
      readPolygons = (map readPolygon).tail.(splitRegex $ mkRegex "<path")

  park_data <- readFile "park.svg"

  let park = readPolygons park_data

  --writeFile "tut1.svg" $ writePolygons (red park)
  let triangulate :: Polygon -> [Polygon]
      triangulate (a:b:c:xs) = [a,b,c]:triangulate (a:c:xs)
      triangulate _ = []                                                

  let polygons = [[(0,0),(100,0),(100,100),(0,100)],
                  [(100,0),(200,0),(200,200),(100,200)],
                  [(200,0),(300,0),(300,300),(200,200)]]
  let triangles = concatMap triangulate polygons
  --writeFile "jj_poly.svg" $ writePolygons (purple polygons)
  --writeFile "jj_tri.svg" $ writePolygons (purple triangles)

  --writeFile "tut2.svg" $ writePolygons (purple triangles)
  
  let clipTriangle :: (Point -> Point -> Point) -> [Point] -> [Point] -> [Polygon] 
      clipTriangle i [] [a,b,c] = [] 
      clipTriangle i [a]  [b,c] = [[a,i a b,i a c]]
      clipTriangle i [a,b]  [c] = [[a,i a c,b],[b,i a c,i b c]]
      clipTriangle i [a,b,c] [] = [[a,b,c]]

  let foo = clipTriangle triangles!! (10.0, 10.0) (10.0, 300.0)
  let slice :: (Point -> Bool) -> (Point -> Point -> Point) -> [Polygon] -> ([Polygon],[Polygon]) 
      slice f i t = (clip f,clip $ not.f)
          where clip g = concatMap ((uncurry $ clipTriangle i).(partition g)) t 
                      
  let sliceX :: Float -> [Polygon] -> ([Polygon],[Polygon]) 
      sliceX x = slice ((x >).fst) interpolateX
          where interpolateX (x1,y1) (x2,y2) = (x,y1+(y2-y1)*(x-x1)/(x2-x1)) 

  let sliceY :: Float -> [Polygon] -> ([Polygon],[Polygon]) 
      sliceY y = slice ((y >).snd) interpolateY
          where interpolateY (x1,y1) (x2,y2) = (x1+(x2-x1)*(y-y1)/(y2-y1),y) 

  let (left_side,right_side) = sliceX 200 triangles

  writeFile "tut3.svg" $ writePolygons $ (red left_side) ++ (blue right_side)
