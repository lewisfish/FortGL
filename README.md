# FortGL

3D software rendering in pure Fortran.

![alt text](https://raw.githubusercontent.com/lewisfish/FortGL/master/output.png "Wireframe head")

## Features so far...
   * Reads .obj and simple .ply file formats
   * Bresenham's line algorithm
   * Rasterisation of triangles
   * Wireframe render of triangles
   * Texture mapping
   * Native output in ppm (with any format available through imagemagick)



![alt text](https://raw.githubusercontent.com/lewisfish/FortGL/master/gourd.png "Wireframe gourd")



![alt text](https://raw.githubusercontent.com/lewisfish/FortGL/master/teapot.png "Wireframe teapot")

## Requirements
  * Only tested on:
  * gfortran 5.4.1+
  * ifort 17.04+
  * pgi 17.4-0 (requires changing all error stops to stops and remove call execute_command_lines)
  * ImageMagick v 6.8.9-9
  * Ubuntu 16.04 LTS

## ToDo
  * make more oop style
  * shaders
  * fix .obj for more than order 3 (i.e. more than just triangles)
  * more file formats (both model input and image output formats)
