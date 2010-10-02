module Main where
import SampleScene
import Raytracer
import Fast_PPM

main = do
	putStrLn "hallo1" 
	save_ppm "./image.ppm" $ render myScene myPlane (200,200)

