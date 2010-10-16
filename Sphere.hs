module Sphere where

import Vec3
import Geometry
import Scene

data  Sphere = Sphere  Position Scalar

instance Surface Sphere where
	intersect s@(Sphere spherePos radius) ray = 
			if ( l2oc < radius ) then -- ray starts inside of sphere
					Just $ findRayExit s ray l2oc
			else
				if ( tca < 0 ) then
					Nothing
				else
					if ( l2hc < 0 ) then
						Nothing
					else 
						Just $ (originR ray) + ((directionR ray) *** (tca - sqrt(l2hc)))
		where
			oc = spherePos - (originR ray)
			l2oc = len'2 oc
			rayDir2 = len'2 (directionR ray)
			tca = (oc *. (directionR ray)) 
			l2hc = ((radius * radius) - l2oc) / rayDir2 + (tca^2)
	normalAt (Sphere spherePos _) pos = pos - spherePos 

findRayExit :: Sphere -> Ray -> Scalar -> Position
findRayExit s@(Sphere spherePos radius) ray l2oc = (originR ray) + ((directionR ray) *** (tca - sqrt(l2hc)))
	where
		oc = spherePos - (originR ray)
		rayDir2 = len'2 $ directionR ray
		tca = (oc *. (directionR ray)) / rayDir2
		l2hc = ((radius * radius) - l2oc) / rayDir2 + (tca^2)

--mkIntersection :: ( Surface a, Material b) => Ray -> Scalar -> Object a b -> Intersection a b
--mkIntersection ray t geo = Intersection ( ((directionR ray) *** t ) + ( originR ray)   ) ray geo

