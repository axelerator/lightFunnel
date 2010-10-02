module Raytracer where

import Data.Maybe
import Data.List (minimumBy)

import Vec3
import Colour

type Position = Vec3
type Direction = Vec3

data Ray = Ray { 
	originR :: Position,
	directionR :: Direction
} deriving (Eq, Show) 

data Intersection = Intersection {
	position :: Position,
	ray :: Ray,
	geo :: Geometry
} deriving (Eq, Show) 

data Color = Color Float Float Float
	deriving (Eq, Show) 

data Scene = Scene {
	geometryS :: [Geometry],
	lightS :: [Light]
} deriving (Eq, Show)

data Material = Const Color
	deriving (Eq, Show)

data Light = SimpleLight Position Color
	deriving (Eq, Show)

data Geometry = Sphere Material Position Float
	deriving (Eq, Show)

type Normal = Vec3

type Intensity = Vec3

mkIntersection :: Ray -> Precision -> Geometry -> Intersection
mkIntersection ray t geo = Intersection ( ((directionR ray) *** t ) + ( originR ray)   ) ray geo

intersect :: Geometry -> Ray -> Maybe Intersection
intersect s@(Sphere _ spherePos radius) ray = 
		if ( l2oc < radius ) then -- ray starts inside of sphere
				Just $ findRayExit s ray l2oc
		else
			if ( tca < 0 ) then
				Nothing
			else
				if ( l2hc < 0 ) then
					Nothing
				else 
					Just $ mkIntersection ray (tca - sqrt(l2hc)) s
	where
		oc = spherePos - (originR ray)
		l2oc = len'2 oc
		rayDir2 = len'2 (directionR ray)
		tca = (oc *. (directionR ray)) 
		l2hc = ((radius * radius) - l2oc) / rayDir2 + (tca^2)

findRayExit :: Geometry -> Ray -> Precision -> Intersection
findRayExit s@(Sphere _ spherePos radius) ray l2oc = mkIntersection ray (tca + sqrt(l2hc)) s
	where
		oc = spherePos - (originR ray)
		rayDir2 = len'2 $ directionR ray
		tca = (oc *. (directionR ray)) / rayDir2
		l2hc = ((radius * radius) - l2oc) / rayDir2 + (tca^2)

data Plane = Plane {
	pOrigin :: Position,
	pU :: Direction,
	pV :: Direction
} deriving (Eq, Show) 

samplePlane :: Plane -> (Integer, Integer) -> [Position]
samplePlane plane (w, h) = [ (pOrigin plane) + (lu *** (fromInteger x)) + (lv *** (fromInteger y)) | y <- [0 .. h-1 ], x <- [0 .. w-1]  ]
	where 
		lu = (pU plane) %% (fromInteger w)
		lv = (pV plane) %% (fromInteger h)


chunk :: Int -> [a] -> [[a]]
chunk i [] = []
chunk i xs = head:(chunk i tail)
	where
		(head, tail) = splitAt i xs

findObsevedSpot :: Scene -> Ray -> Maybe Intersection
findObsevedSpot = undefined 

mkRays :: Plane -> (Integer, Integer) -> [Ray]
mkRays plane res = [Ray o normal | o <- (samplePlane plane res)]
		where normal = normalize $ ((pU plane) *# (pV plane))

shade :: Scene -> Maybe Intersection -> Color
shade _ Nothing = Color 0.0 0.0 0.0
shade scene (Just i) = toColor $ foldl (+) (Vec3 0.0 0.0 0.0) $ map (phong i) (lightS scene) 
	where
		Sphere (Const col)  _ _ = geo i  

toColor :: Intensity -> Color
toColor (Vec3 r g b) = Color r g b

phong :: Intersection -> Light -> Intensity
phong i (SimpleLight lightPos (Color r g b))  = Vec3 (r*sr*phongTerm) (g*sg*phongTerm) (b*sb*phongTerm)
	where
		iToLight = lightPos - (position i)
		rayDir = directionR (ray i)
		n = normal i
		reflect = (iToLight *. n) * 2.0
		phongDir = iToLight - (n *** reflect  ) 
		phongTerm = max 0.0 (phongDir *. rayDir)
		Sphere (Const (Color sr sg sb)) _ _ = geo i 
		
normal :: Intersection -> Normal
normal i = normalize $ iPos - sPos
	where
		(Sphere _ sPos _ ) = geo i
		iPos = position i


visibility :: Scene -> [Ray] -> [Maybe Intersection]
visibility scene rays = map (raySceneIntersections geos) rays
	where geos = geometryS scene

raySceneIntersections :: [Geometry] -> Ray -> Maybe Intersection
raySceneIntersections gs r = 	if (null allIntersects) then
																Nothing
															else
																Just $ minimumBy minFunc allIntersects
	where
		allIntersects = catMaybes $ map (\g -> intersect g r) gs
		minFunc p1 p2 = if ((len'2 (orig - (position p1))) < (len'2 (orig - (position p2)))) then
											LT
										else
											GT
		orig = (originR r)		


render :: Scene -> Plane -> (Integer, Integer) -> [[Colour]]
render scene plane resolution@(w,h) = chunk (fromInteger w) $ map export colored
	where
		samples = mkRays plane resolution
		intersections = visibility scene samples
		colored = map (shade scene) intersections

export :: Color -> Colour
export (Color r g b) = Colour (realToFrac r) (realToFrac g) (realToFrac b )

