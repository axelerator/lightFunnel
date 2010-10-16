module Scene where

import Vec3
import Geometry

class Surface a where
	intersect :: a -> Ray -> Maybe Position
	normalAt :: a -> Position -> Direction 

class Material a where
--	shade :: (Surface b) =>  Intersection b a -> Light -> Intensity
	shade :: a -> Position -> Normal -> Ray -> Light -> Intensity

data Color = Color Float Float Float
	deriving (Eq, Show) 

data (Surface a, Material b) => Scene a b = Scene {
	objectS :: [Object a b],
	lightS :: [Light]
} deriving (Eq, Show)

data Light = SimpleLight Position Color
	deriving (Eq, Show)

data (Surface a, Material b) => Object a b = Object {
 surfaceO :: a,
 materialO :: b  
}	deriving (Eq, Show)

type Intensity = Vec3


data (Surface a, Material b) => Intersection a b = Intersection {
	position :: Position,
	ray :: Ray,
	object :: Object a b 
} deriving (Eq, Show) 


