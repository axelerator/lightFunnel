module SampleScene where
import Vec3
import Raytracer

constRed = Const $ Color 1.0 0.0 0.0
constBlue = Const $ Color 0.0 0.0 1.0
constGreen = Const $ Color 0.0 1.0 0.0
constWhite = Color 1.0 1.0 1.0
halfWhite = Color 0.3 0.3 0.3
myPlane = Plane (Vec3  (-0.5) 0.5 0.5) (Vec3  1.0 0.0 0.0) (Vec3  0.0 (-1.0) 0.0) 
unitSphere = Sphere constRed (Vec3 0.0 0.0 (-1.0)) 0.25
blueLeft = Sphere constBlue (Vec3 (-0.45) 0.10 (-0.75)) 0.09
greenRight = Sphere constGreen (Vec3 (0.45) 0.0 (-0.75)) 0.09
redTop = Sphere constRed (Vec3 (0.0) 0.3 (-0.75)) 0.09

light1 = SimpleLight (Vec3 0.0 2.0 (-1.0)) constWhite
light2 = SimpleLight (Vec3 (-0.30) (-2.0) (-1.0)) halfWhite

myScene = Scene [unitSphere,redTop,greenRight,blueLeft] [light2,light1]
