#! /bin/bash
rm image.ppm image.png 
runghc Main.hs && convert image.ppm image.png && /Applications/Preview.app/Contents/MacOS/Preview image.png
