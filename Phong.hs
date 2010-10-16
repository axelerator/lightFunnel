module Phong where

import Vec3
import Geometry
import Scene

data PhongMaterial = PhongMaterial {
	ambient :: Color,
	specular :: Color,
	diffuse :: Color
} deriving ( Eq, Show)

instance Material PhongMaterial where
	shade mat iPosition n ray (SimpleLight lightPos (Color r g b)) = Vec3 (r*sr*phongTerm) (g*sg*phongTerm) (b*sb*phongTerm)
		where
			iToLight = lightPos - iPosition 
			rayDir = directionR ray
			reflect = (iToLight *. n) * 2.0
			phongDir = iToLight - (n *** reflect  ) 
			phongTerm = max 0.0 (phongDir *. rayDir)
			(Color sr sg sb) = diffuse mat 

