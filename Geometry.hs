module Geometry where

import Vec3



type Position = Vec3
type Direction = Vec3

type Normal = Vec3
data Ray = Ray { 
	originR :: Position,
	directionR :: Direction
} deriving (Eq, Show) 

data Plane = Plane {
	pOrigin :: Position,
	pU :: Direction,
	pV :: Direction
} deriving (Eq, Show) 


