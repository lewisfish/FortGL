# FortGL

3D software rendering in pure Fortran.

![alt text](https://raw.githubusercontent.com/lewisfish/FortGL/master/output.png "Texture mapped head")

## Features so far...
   * Reads .obj and simple .ply file formats
   * Bresenham's line algorithm
   * Rasterisation of triangles
   * User defined shaders
   * Movable camera and perspective view
   * ~~Wireframe render of triangles~~ currently broken
   * ~~Texture mapping~~ not implmented anymore
   * Native output in ppm (with any format available through imagemagick)



![alt text](https://raw.githubusercontent.com/lewisfish/FortGL/master/gourd.png "Wireframe gourd")



![alt text](https://raw.githubusercontent.com/lewisfish/FortGL/master/outputtoon.png "Example shader")

## Requirements
  * Only tested on:
  * gfortran 5.4.1+
  * ifort 17.04+
  * pgi 17.4-0 (requires changing all error stops to stops and remove call execute_command_lines)
  * ImageMagick v 6.8.9-9
  * Ubuntu 16.04 LTS

## ToDo
  * re implment wire render and texture mapping
  * normal mapping
  * fix .obj for more than order 3 (i.e. more than just triangles)
  * more file formats (both model input and image output formats)
