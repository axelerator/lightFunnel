module Raytracer where

import Data.Maybe
import Data.List (minimumBy)

import Vec3
import Geometry
import Colour

import Scene
import Phong
import Sphere



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

mkRays :: Plane -> (Integer, Integer) -> [Ray]
mkRays plane res = [Ray o normal | o <- (samplePlane plane res)]
		where normal = normalize $ ((pU plane) *# (pV plane))

shadeAll :: (Surface s, Material m) => Scene s m -> Maybe (Intersection s m) -> Color
shadeAll _ Nothing = Color 0.0 0.0 0.0
shadeAll scene (Just i) = toColor $ foldl (+) (Vec3 0.0 0.0 0.0) $ map (shadeIntersection i) (lightS scene) 

shadeIntersection :: (Surface s, Material m) => Intersection s m -> Light -> Intensity
shadeIntersection i l = shade mat pos normal r l  
	where
		pos = position i
		normal = normalAt (surfaceO (object i)) pos
		r = ray i 
		mat = materialO (object i)

toColor :: Intensity -> Color
toColor (Vec3 r g b) = Color r g b

visibility :: (Surface s, Material m) => Scene s m -> [Ray] -> [Maybe (Intersection s m)]
visibility scene rays = map (raySceneIntersections geos) rays
	where geos = objectS scene

raySceneIntersections :: (Surface s, Material m) => [(Object s m)] -> Ray -> Maybe (Intersection s m)
raySceneIntersections gs r = 	if (null allIntersects) then
																Nothing
															else
																Just $ minimumBy minFunc allIntersects
	where
		--allIntersects = catMaybes $ map (\g -> Intersection g r) gs
		allIntersects = catMaybes $ map (intersectWithObject r) gs
		minFunc p1 p2 = if ((len'2 (orig - (position p1))) < (len'2 (orig - (position p2)))) then
											LT
										else
											GT
		orig = (originR r)		
		mkIntrsect pos = Intersection pos r

intersectWithObject :: (Surface s, Material m) => Ray -> Object s m -> Maybe (Intersection s m)
intersectWithObject ray o = if (isNothing maybeIntersected) then
												Nothing
											else
												Just (Intersection (fromJust maybeIntersected) ray o)
	where
		maybeIntersected = intersect (surfaceO o) ray


render :: (Surface s, Material m) => Scene s m -> Plane -> (Integer, Integer) -> [[Colour]]
render scene plane resolution@(w,h) = chunk (fromInteger w) $ map export colored
	where
		samples = mkRays plane resolution
		intersections = visibility scene samples
		colored = map (shadeAll scene) intersections

export :: Color -> Colour
export (Color r g b) = Colour (realToFrac r) (realToFrac g) (realToFrac b )

