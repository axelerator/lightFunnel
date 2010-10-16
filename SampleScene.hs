module SampleScene where
import Vec3
import Scene
import Raytracer
import Geometry
import Sphere
import Phong

constRed = simpleCol $ Color 1.0 0.0 0.0
constBlue = simpleCol $ Color 0.0 0.0 1.0
constGreen = simpleCol $ Color 0.0 1.0 0.0
constWhite = Color 1.0 1.0 1.0
halfWhite = Color 0.3 0.3 0.3
myPlane = Plane (Vec3  (-0.5) 0.5 0.5) (Vec3  1.0 0.0 0.0) (Vec3  0.0 (-1.0) 0.0) 
unitSphere = Object (Sphere (Vec3 0.0 0.0 (-1.0)) 0.25) constRed 
blueLeft = Object (Sphere (Vec3 (-0.45) 0.10 (-0.75)) 0.09) constBlue 
greenRight = Object (Sphere (Vec3 (0.45) 0.0 (-0.75)) 0.09) constGreen 
redTop = Object (Sphere (Vec3 (0.0) 0.3 (-0.75)) 0.09) constRed 

light1 = SimpleLight (Vec3 0.0 2.0 (-1.0)) constWhite
light2 = SimpleLight (Vec3 (-0.30) (-2.0) (-1.0)) halfWhite

simpleCol = PhongMaterial (Color 1.0 1.0 1.0) (Color 1.0 1.0 1.0)


myScene = Scene [unitSphere,redTop,greenRight,blueLeft] [light2,light1]
