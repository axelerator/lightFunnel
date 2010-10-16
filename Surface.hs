module Sphere where

import Vec3
import Geometry
import Material


data Sphere = Sphere Material Position Scalar

instance Surface Sphere where
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


