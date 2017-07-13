Module Draw

   use Image   
   
   implicit none
   
   type point
      integer :: x, y
   end type point

   type bucket
      integer :: yMax, yMin, x, sign, dx, dy, summ
   end type bucket
   
   interface draw_line
      module procedure draw_lineRGB
      module procedure draw_lineRGBA
   end interface draw_line

   interface draw_rectangle
      module procedure draw_rectangleRGB
      module procedure draw_rectangleRGB_A
      module procedure draw_rectangleRGBA
   end interface

   interface draw_circle
      module procedure draw_circleRGB
      ! module procedure draw_circleRGB_A
      module procedure draw_circleRGBA
   end interface

   interface draw_triangle
      ! module procedure draw_triangleRGB
      module procedure draw_triangleRGBA
   end interface draw_triangle

   interface flood_fill
      module procedure flood_fillRGB
      module procedure flood_fillRGBA
   end interface

   interface swap
      module procedure swap_I
   end interface

   interface operator(-)
      module procedure pointSub
   end interface

   interface operator(+)
      module procedure pointAdd
   end interface

   interface operator(*)
      module procedure pointmultscal
   end interface
   

   private
   public :: flood_fill, draw_line, draw_circle, draw_rectangle, point, draw_triangle

Contains

   subroutine draw_lineRGB(img, p1, p2, colour)
   
      implicit none
      
      type(RGBimage),      intent(INOUT) :: img
      type(RGB), optional, intent(IN)    :: colour
      type(point),         intent(IN)    :: p1, p2
      type(RGB)                          :: c
      type(point)                        :: p1t, p2t
      integer                            :: x, y, dx, dy, error, ystep
      logical                            :: steep
      
      if(.not.present(colour))then
         c = RGB(0, 0, 0)
      else
         c = colour
      end if

      p1t = p1
      p2t = p2
      
      steep = (abs(p2t%y - p1t%y) > abs(p2t%x -p1t%x))
   
      if(steep)then
         call swap(p1t%x, p1t%y)
         call swap(p2t%x, p2t%y)
      end if
      
      if(p1t%x > p2t%x)then
         call swap(p1t%x, p2t%x)
         call swap(p1t%y, p2t%y)
      end if
      
      dx = p2t%x - p1t%x
      dy = abs(p2t%y - p1t%y)
      error = dx/2
      y = p1t%y
      
      if(p1t%y < p2t%y)then
         ystep = 1
      else
         ystep = -1
      end if
      
    do x = p1t%x, p2t%x
       if (steep) then
          call set_pixel(img, y, x, c)
       else 
          call set_pixel(img, x, y, c)
       end if
       error = error - dy
       if ( error < 0 ) then
          y = y + ystep
          error = error + dx
       end if
    end do    
   end subroutine draw_lineRGB


   subroutine draw_lineRGBA(img, p1, p2, colour)
   
      implicit none
      
      type(RGBAimage),      intent(INOUT) :: img
      type(RGBA), optional, intent(IN)    :: colour
      type(point),         intent(IN)    :: p1, p2
      type(RGBA)                          :: c
      type(point)                        :: p1t, p2t
      integer                            :: x, y, dx, dy, error, ystep
      logical                            :: steep
      
      if(.not.present(colour))then
         c = RGBA(0, 0, 0, 0)
      else
         c = colour
      end if

      p1t = p1
      p2t = p2
      
      steep = (abs(p2t%y - p1t%y) > abs(p2t%x -p1t%x))
   
      if(steep)then
         call swap(p1t%x, p1t%y)
         call swap(p2t%x, p2t%y)
      end if
      
      if(p1t%x > p2t%x)then
         call swap(p1t%x, p2t%x)
         call swap(p1t%y, p2t%y)
      end if
      
      dx = p2t%x - p1t%x
      dy = abs(p2t%y - p1t%y)
      error = dx/2
      y = p1t%y
      
      if(p1t%y < p2t%y)then
         ystep = 1
      else
         ystep = -1
      end if
      
    do x = p1t%x, p2t%x
       if (steep) then
          call set_pixel(img, y, x, c)
       else 
          call set_pixel(img, x, y, c)
       end if
       error = error - dy
       if ( error < 0 ) then
          y = y + ystep
          error = error + dx
       end if
    end do    
   end subroutine draw_lineRGBA


   subroutine draw_rectangleRGB(img, p1, p2, colour, fill)
   
      implicit none
      
      type(RGBimage),    intent(INOUT) :: img
      type(RGB),         intent(IN)    :: colour
      type(point),       intent(INOUT) :: p1, p2
      logical, optional, intent(IN)    :: fill
      integer                          :: i, j
      logical                          :: flag, xf, yf
      
      if(present(fill))then
         if(fill)then
            flag = .TRUE.
         else
            flag = .FALSE.
         end if
      else
         flag = .FALSE.
      end if
      
      if(p1%x > p2%x)then
         call swap(p1%x, p2%x)
         call swap(p1%y, p2%y)
         xf = .TRUE.
      end if
      
      if(p1%y > p2%y.and..not.xf)then
         call swap(p1%y, p2%y)
         yf = .TRUE.
      end if
      
      if(flag)then
         do j = p1%y , p2%y
            do i = p1%x , p2%x
               call set_pixel(img, i, j, colour)
            end do
         end do
      else
         do i = p1%x , p2%x
            call set_pixel(img, i, p1%y, colour)
            call set_pixel(img, i, p2%y, colour)
         end do
         
         do i = p1%y , p2%y
            call set_pixel(img, p1%x, i, colour)
            call set_pixel(img, p2%x, i, colour)
         end do
      end if
      
      if(xf)then
         call swap(p1%x, p2%x)
         call swap(p1%y, p2%y)
      elseif(yf.and..not.xf)then
         call swap(p1%x, p2%x)
         call swap(p1%y, p2%y)
      end if
   end subroutine draw_rectangleRGB


   subroutine draw_rectangleRGB_A(img, p1, p2, colour, fill)
   
      implicit none
      
      type(RGBAimage),    intent(INOUT) :: img
      type(RGB),         intent(IN)     :: colour
      type(point),       intent(INOUT)  :: p1, p2
      logical, optional, intent(IN)     :: fill
      type(RGBA)                        :: c_a
      integer                           :: i, j
      logical                           :: flag, xf, yf
      
      c_a = RGBA(colour%red, colour%green, colour%green, 255)

      if(present(fill))then
         if(fill)then
            flag = .TRUE.
         else
            flag = .FALSE.
         end if
      else
         flag = .FALSE.
      end if
      
      if(p1%x > p2%x)then
         call swap(p1%x, p2%x)
         call swap(p1%y, p2%y)
         xf = .TRUE.
      end if
      
      if(p1%y > p2%y.and..not.xf)then
         call swap(p1%y, p2%y)
         yf = .TRUE.
      end if
      
      if(flag)then
         do j = p1%y , p2%y
            do i = p1%x , p2%x
               call set_pixel(img, i, j, c_a)
            end do
         end do
      else
         do i = p1%x , p2%x
            call set_pixel(img, i, p1%y, c_a)
            call set_pixel(img, i, p2%y, c_a)
         end do
         
         do i = p1%y , p2%y
            call set_pixel(img, p1%x, i, c_a)
            call set_pixel(img, p2%x, i, c_a)
         end do
      end if
      
      if(xf)then
         call swap(p1%x, p2%x)
         call swap(p1%y, p2%y)
      elseif(yf.and..not.xf)then
         call swap(p1%x, p2%x)
         call swap(p1%y, p2%y)
      end if
   end subroutine draw_rectangleRGB_A


   subroutine draw_rectangleRGBA(img, p1, p2, colour, fill, blend)
   
      implicit none
      
      type(RGBAimage),   intent(INOUT) :: img
      type(RGBA),        intent(IN)    :: colour
      type(point),       intent(INOUT) :: p1, p2
      logical, optional, intent(IN)    :: fill, blend
      type(RGBA)                       :: c1, c2
      integer                          :: i, j
      logical                          :: flag_f, flag_b, xf, yf
      
      if(present(blend))then
         if(blend)then
            flag_b = .TRUE.
         else
            flag_b = .FALSE.
         end if
      else
         flag_b = .FALSE.
      end if

      if(present(fill))then
         if(fill)then
            flag_f = .TRUE.
         else
            flag_f = .FALSE.
         end if
      else
         flag_f = .FALSE.
      end if
      
      if(p1%x > p2%x)then
         call swap(p1%x, p2%x)
         call swap(p1%y, p2%y)
         xf = .TRUE.
      end if
      
      if(p1%y > p2%y.and..not.xf)then
         call swap(p1%y, p2%y)
         yf = .TRUE.
      end if
      
      if(flag_f.and.flag_b)then
         do j = p1%y , p2%y
            do i = p1%x , p2%x
               call get_pixel(img, i, j, c1)
               c2 = alpha_comp(colour, c1)
               call set_pixel(img, i, j, c2)
            end do
         end do
      elseif(flag_f.and..not.flag_b)then
         do j = p1%y , p2%y
            do i = p1%x , p2%x
               call set_pixel(img, i, j, colour)
            end do
         end do
      else
         do i = p1%x , p2%x
            call set_pixel(img, i, p1%y, colour)
            call set_pixel(img, i, p2%y, colour)
         end do
         
         do i = p1%y , p2%y
            call set_pixel(img, p1%x, i, colour)
            call set_pixel(img, p2%x, i, colour)
         end do
      end if
      
      if(xf)then
         call swap(p1%x, p2%x)
         call swap(p1%y, p2%y)
      elseif(yf.and..not.xf)then
         call swap(p1%x, p2%x)
         call swap(p1%y, p2%y)
      end if
   end subroutine draw_rectangleRGBA


   subroutine draw_circleRGB(img, p, radius, colour, fill)

      implicit none
      
      type(RGBimage),    intent(INOUT) :: img
      type(RGB),         intent(IN)    :: colour
      type(point),       intent(IN)    :: p
      integer,           intent(IN)    :: radius
      logical, optional, intent(IN)    :: fill
      integer                          :: x, y, error
      
      x = radius
      y = 0
      error = 0
      
      do while( x >= y)
    
         call set_pixel(img, p%x + x, p%y + y , colour)
         call set_pixel(img, p%x + y, p%y + x , colour)
         call set_pixel(img, p%x - y, p%y + x , colour)
         call set_pixel(img, p%x - x, p%y + y , colour)
         call set_pixel(img, p%x - x, p%y - y , colour)
         call set_pixel(img, p%x - y, p%y - x , colour)
         call set_pixel(img, p%x + y, p%y - x , colour)
         call set_pixel(img, p%x + x, p%y - y , colour)
         
         y = y + 1
         error = error + 1 + 2*y
         if(2*(error-x) + 1 > 0)then
            x = x - 1
            error = error + (1 - 2*x)
         end if
      end do
      
      if(present(fill))then
         if(fill)then
            call flood_fill(img, p%x, p%y, colour, RGB(img%Red(x, y), img%Green(x, y), img%Blue(x, y)))
         end if
      end if

   end subroutine draw_circleRGB


   subroutine draw_circleRGBA(img, p, radius, colour, fill, blend)

      implicit none
      
      type(RGBAimage),   intent(INOUT) :: img
      type(RGBA),        intent(IN)    :: colour
      type(point),       intent(IN)    :: p
      integer,           intent(IN)    :: radius
      logical, optional, intent(IN)    :: fill, blend
      type(RGBA)                       :: c1, c2
      integer                          :: x, y, error,i,j
      
      x = radius
      y = 0
      error = 0
      c1 = colour

      if(present(fill).and.present(blend))then
         if(fill.and.blend)then

            do i = -radius, radius, 1
               do j = -radius, radius, 1
                  if(i*i + j*j <= radius*radius)then
                     call get_pixel(img, p%x+i, p%y+j, c2)
                     c1 = alpha_comp(colour, c2)
                     call set_pixel(img, p%x+i, p%y+j, c1)
                  end if
               end do
            end do
         elseif(fill)then
            do i = -radius, radius, 1
               do j = -radius, radius, 1
                  if(i*i + j*j <= radius*radius)then
                     call set_pixel(img, p%x+i, p%y+j, c1)
                  end if
               end do
            end do
         elseif(blend)then
            do while(x >= y)

               call get_pixel(img, p%x+x, p%y+y, c2)
               c1 = alpha_comp(colour, c2)
               call set_pixel(img, p%x + x, p%y + y , c1)

               call get_pixel(img, p%x+y, p%y+x, c2)
               c1 = alpha_comp(colour, c2)
               call set_pixel(img, p%x + y, p%y + x , c1)

               call get_pixel(img, p%x-y, p%y+x, c2)
               c1 = alpha_comp(colour,c2)
               call set_pixel(img, p%x - y, p%y + x , c1)

               call get_pixel(img, p%x-x, p%y+y, c2)
               c1 = alpha_comp(colour, c2)
               call set_pixel(img, p%x - x, p%y + y , c1)

               call get_pixel(img, p%x-x, p%y-y, c2)
               c1 = alpha_comp(colour, c2)
               call set_pixel(img, p%x - x, p%y - y , c1)

               call get_pixel(img, p%x-y, p%y-x, c2)
               c1 = alpha_comp(colour, c2)
               call set_pixel(img, p%x - y, p%y - x , c1)

               call get_pixel(img, p%x+y, p%y-x, c2)
               c1 = alpha_comp(colour, c2)
               call set_pixel(img, p%x + y, p%y - x , c1)

               call get_pixel(img, p%x+x, p%y-y, c2)
               c1 = alpha_comp(colour, c2)
               call set_pixel(img, p%x + x, p%y - y , c1)

               y = y + 1
               error = error + 1 + 2*y
               if(2*(error-x) + 1 > 0)then
                  x = x - 1
                  error = error + (1 - 2*x)
               end if
            end do
         else
            do while(x >= y)

               call set_pixel(img, p%x + x, p%y + y , c1)
               call set_pixel(img, p%x + y, p%y + x , c1)
               call set_pixel(img, p%x - y, p%y + x , c1)
               call set_pixel(img, p%x - x, p%y + y , c1)
               call set_pixel(img, p%x - x, p%y - y , c1)
               call set_pixel(img, p%x - y, p%y - x , c1)
               call set_pixel(img, p%x + y, p%y - x , c1)
               call set_pixel(img, p%x + x, p%y - y , c1)

               y = y + 1
               error = error + 1 + 2*y
               if(2*(error-x) + 1 > 0)then
                  x = x - 1
                  error = error + (1 - 2*x)
               end if
            end do
         end if
      elseif(present(fill))then
         if(fill)then
            do i = -radius, radius, 1
               do j = -radius, radius, 1
                  if(i*i + j*j <= radius*radius)then
                     call get_pixel(img, p%x+i, p%y+j, c2)
                     c1 = alpha_comp(colour, c2)
                     call set_pixel(img, p%x+i, p%y+j, c1)
                  end if
               end do
            end do
         else
            do while( x >= y)
    
               call set_pixel(img, p%x + x, p%y + y , colour)
               call set_pixel(img, p%x + y, p%y + x , colour)
               call set_pixel(img, p%x - y, p%y + x , colour)
               call set_pixel(img, p%x - x, p%y + y , colour)
               call set_pixel(img, p%x - x, p%y - y , colour)
               call set_pixel(img, p%x - y, p%y - x , colour)
               call set_pixel(img, p%x + y, p%y - x , colour)
               call set_pixel(img, p%x + x, p%y - y , colour)
         
               y = y + 1
               error = error + 1 + 2*y
               if(2*(error-x) + 1 > 0)then
                  x = x - 1
                  error = error + (1 - 2*x)
               end if
            end do
         end if
      else
         do while( x >= y)
    
               call set_pixel(img, p%x + x, p%y + y , colour)
               call set_pixel(img, p%x + y, p%y + x , colour)
               call set_pixel(img, p%x - y, p%y + x , colour)
               call set_pixel(img, p%x - x, p%y + y , colour)
               call set_pixel(img, p%x - x, p%y - y , colour)
               call set_pixel(img, p%x - y, p%y - x , colour)
               call set_pixel(img, p%x + y, p%y - x , colour)
               call set_pixel(img, p%x + x, p%y - y , colour)
         
               y = y + 1
               error = error + 1 + 2*y
               if(2*(error-x) + 1 > 0)then
                  x = x - 1
                  error = error + (1 - 2*x)
               end if
         end do   
      end if
   end subroutine draw_circleRGBA


   type(vector) function barycentric(a, b, c, p)

      use types

      implicit none

      type(ivec) :: a, b, c, s(2), u, p

      s(1)%x = c%x - a%x 
      s(1)%y = b%x - a%x 
      s(1)%z = a%x - p%x 

      s(2)%x = c%y - a%y 
      s(2)%y = b%y - a%y 
      s(2)%z = a%y - p%y

      u = s(1) .cross. s(2)

      if(abs(u%z) < 1)then
         barycentric = vector(-1.,1.,1.)
         return
      else
         barycentric = vector(1.0-(u%x+u%y)/real(u%z), real(u%y)/real(u%z), real(u%x)/real(u%z))
      end if
   end function barycentric


   subroutine draw_triangleRGBA(img, pts, zbuffer, intensity, colour, texture, uvs, norms, light, wire)

      use triangleclass
      use types

      implicit none

      type(RGBAimage),           intent(INOUT) :: img
      type(RGBAimage), optional, intent(IN)    :: texture
      type(RGBA),      optional, intent(IN)    :: colour
      type(ivec),                intent(INOUT) :: pts(:)
      type(vector),    optional, intent(IN)    :: uvs(:)
      type(vector),optional :: norms(:), light
      logical,         optional, intent(IN)    :: wire
      real,                      intent(INOUT) :: zbuffer(:)
      real,                      intent(INOUT) :: intensity

      type(vector) :: uv, n
      type(RGBA) :: c

      type(vector) :: tmp
      type(ivec)   :: p
      integer      :: bmin(2), bmax(2), clamp(2), i, j, k
      real         :: bc_screen(3)

      if(.not. present(wire))then
         bmin = [img%width, img%height]
         bmax = [0, 0]
         clamp = [img%width, img%height]

         !get bounding box for triangle
         do i = 1, 3
               bmin(1) = min(bmin(1), pts(i)%x)
               bmin(2) = min(bmin(2), pts(i)%y)

               bmax(1) = min(clamp(1), max(bmax(1), pts(i)%x))
               bmax(2) = min(clamp(2), max(bmax(2), pts(i)%y))
         end do

         do i = bmin(1), bmax(1)
            do j = bmin(2), bmax(2)
               p%x = i
               p%y = j
               p%z = 0

               tmp = barycentric(pts(1), pts(2), pts(3), p)
               bc_screen = [tmp%x, tmp%y, tmp%z]

               if(bc_screen(1) < 0. .or. bc_screen(2) < 0. .or. bc_screen(3) < 0.)cycle
               do k = 1, 3
                  p%z = p%z + int(pts(k)%z*bc_screen(k))
               end do

               if(zbuffer(int(p%x+p%y*img%width)) < p%z)then
                  zbuffer(int(p%x+p%y*img%width)) = p%z
                  if(present(texture))then
                     if(.not. present(uvs))error stop "Need uvs"
                     !interpolate uv corrds
                     uv = uvs(1)*tmp%x + uvs(2)*tmp%y + uvs(3)*tmp%z
                     n = norms(1)*tmp%x + norms(2)*tmp%y + norms(3)*tmp%z
                     n = normal(n)
                     intensity = abs( n .dot. light)
                     !get texture colour
                     call get_pixel(texture, int(uv%x), int(uv%y), c)
                     !add lighting
                     c = c * intensity
                     call set_pixel(img, p%x, p%y, c)
                  else
                     call set_pixel(img, p%x, p%y, colour)
                  end if
               end if
            end do
         end do
      else
         if(wire)then
            !wireframe render
            call draw_line(img, point(pts(1)%x, pts(1)%y), point(pts(2)%x, pts(2)%y), RGBA(255,255,255,255))
            call draw_line(img, point(pts(2)%x, pts(2)%y), point(pts(3)%x, pts(3)%y), RGBA(255,255,255,255))
            call draw_line(img, point(pts(3)%x, pts(3)%y), point(pts(1)%x, pts(1)%y), RGBA(255,255,255,255))
         else
            error stop 1
         end if
      end if
   end subroutine draw_triangleRGBA


   subroutine swap_I(a, b)
   
      implicit none
      
      integer, intent(INOUT) :: a, b
      integer                :: tmp
      
      tmp = a
      a = b
      b = tmp
      
   end subroutine swap_I
   

   recursive subroutine flood_fillRGB(img, x, y, colour, old)
   
      implicit none
      
      type(RGBimage), intent(INOUT) :: img
      type(RGB),      intent(IN)    :: colour, old
      integer,        intent(IN)    :: x, y
      integer                       :: x1

      if(old == colour)return
      if( (RGB(img%Red(x, y), img%Green(x, y), img%Blue(x, y)) /= old))return

      x1 = x
      do while(x1 < img%width .and. RGB(img%Red(x1, y), img%Green(x1, y), & 
                                             img%Blue(x1, y)) == old)
         call set_pixel(img, x1, y, colour)
         x1 = x1 + 1
      end do
      
      x1 = x - 1
      if(x1/=0)then
         do while(x1 >= 1 .and. RGB(img%Red(x1, y), img%Green(x1, y), & 
                                                img%Blue(x1, y)) == old)
            call set_pixel(img, x1, y, colour)

            x1 = x1 - 1
            if(x1==0)exit
         end do
      end if
      
!      move up
      x1 = x
      do while(x1 < img%width .and. RGB(img%Red(x1, y), img%Green(x1, y), & 
                                             img%Blue(x1, y)) == colour)
         if(y==1)exit
         if(y > 1 .and. RGB(img%Red(x1, y-1), img%Green(x1, y-1), & 
                                             img%Blue(x1, y-1))==old)then
            call flood_fill(img, x, y-1, colour, old)
         end if
         x1 = x1 +1

      end do

      x1 = x - 1
      if(x1/=0)then
         do while(x1 >= 1 .and. RGB(img%Red(x1, y), img%Green(x1, y), & 
                                                img%Blue(x1, y)) == colour)
            if(x1==0 .or. y==1)exit
            if(y > 1 .and. RGB(img%Red(x1, y-1), img%Green(x1, y-1), & 
                                                img%Blue(x1, y-1))==old)then
               call flood_fill(img, x, y-1, colour, old)
            end if
            x1 = x1 - 1
            if(x1==0 .or. y==0)exit
         end do
      end if
      !move down
      
      x1 = x
      do while(x1 < img%width .and. RGB(img%Red(x1, y), img%Green(x1, y), & 
                                             img%Blue(x1, y)) == colour)
         if(y==img%height)exit
         if(y < img%height-1 .and. RGB(img%Red(x1, y+1), img%Green(x1, y+1), & 
                                             img%Blue(x1, y+1))==old)then
            call flood_fill(img, x, y+1, colour, old)
         end if
         x1 = x1 +1
      end do
      
      x1 = x-1
      if(x1/=0)then
         do while(x1 >= 1 .and. RGB(img%Red(x1, y), img%Green(x1, y), & 
                                                img%Blue(x1, y)) == colour)
            if(y==img%height)exit
            if(y < img%height .and. RGB(img%Red(x1, y+1), img%Green(x1, y+1), & 
                                                img%Blue(x1, y+1))==old)then
               call flood_fill(img, x, y+1, colour, old)
            end if
            x1 = x1 - 1
            if(x1==0.or.y==0)exit
         end do
      end if
   end subroutine flood_fillRGB


   recursive subroutine flood_fillRGBA(img, x, y, colour, old)
   
      implicit none
      
      type(RGBAimage), intent(INOUT) :: img
      type(RGBA),      intent(IN)    :: colour, old
      integer,         intent(IN)    :: x, y
      type(RGBA)                     :: c
      integer                        :: x1

      if(old == colour)return
      call get_pixel(img, x, y, c)
      if(c /= old)return
      x1 = x
      do while(x1 < img%width .and. RGBA(img%Red(x1, y), img%Green(x1, y), & 
                                         img%Blue(x1, y), img%alpha(x1, y)) == old)
         call set_pixel(img, x1, y, colour)
         x1 = x1 + 1
      end do
      
      x1 = x - 1
      if(x1/=0)then
         do while(x1 >= 1 .and. RGBA(img%Red(x1, y), img%Green(x1, y), & 
                                     img%Blue(x1, y), img%alpha(x1, y)) == old)
            call set_pixel(img, x1, y, colour)

            x1 = x1 - 1
            if(x1==0)exit
         end do
      end if
      
!      move up
      x1 = x
      do while(x1 < img%width .and. RGBA(img%Red(x1, y), img%Green(x1, y), & 
                                         img%Blue(x1, y), img%alpha(x1, y)) == colour)
         if(y==1)exit
         if(y > 1 .and. RGBA(img%Red(x1, y-1), img%Green(x1, y-1), & 
                             img%Blue(x1, y-1), img%alpha(x1, y-1)) == old)then
            call flood_fill(img, x, y-1, colour, old)
         end if
         x1 = x1 +1

      end do

      x1 = x - 1
      if(x1/=0)then
         do while(x1 >= 1 .and. RGBA(img%Red(x1, y), img%Green(x1, y), & 
                                     img%Blue(x1, y), img%alpha(x1, y)) == colour)
            if(x1==0 .or. y==1)exit
            if(y > 1 .and. RGBA(img%Red(x1, y-1), img%Green(x1, y-1), & 
                                img%Blue(x1, y-1), img%alpha(x1, y-1)) == old)then
               call flood_fill(img, x, y-1, colour, old)
            end if 
            x1 = x1 - 1
            if(x1==0 .or. y==0)exit
         end do
      end if
      !move down
      
      x1 = x
      do while(x1 < img%width .and. RGBA(img%Red(x1, y), img%Green(x1, y), & 
                                         img%Blue(x1, y), img%alpha(x1, y)) == colour)
         if(y==img%height)exit
         if(y < img%height-1 .and. RGBA(img%Red(x1, y+1), img%Green(x1, y+1), & 
                                        img%Blue(x1, y+1), img%alpha(x1, y+1)) == old)then
            call flood_fill(img, x, y+1, colour, old)
         end if
         x1 = x1 +1
      end do
      
      x1 = x-1
      if(x1/=0)then
         do while(x1 >= 1 .and. RGBA(img%Red(x1, y), img%Green(x1, y), & 
                                     img%Blue(x1, y), img%alpha(x1, y)) == colour)
            if(y==img%height)exit
            if(y < img%height .and. RGBA(img%Red(x1, y+1), img%Green(x1, y+1), & 
                                         img%Blue(x1, y+1), img%alpha(x1, y+1)) == old)then
               call flood_fill(img, x, y+1, colour, old)
            end if
            x1 = x1 - 1
            if(x1==0.or.y==0)exit
         end do
      end if

   end subroutine flood_fillRGBA


   function alpha_comp(ca, cb) result(co)

        implicit none

        type(RGBA), intent(IN) :: cb, ca
        type(RGBA)             :: co
        real                   :: a_tmp, b_tmp

        a_tmp = ca%alpha/255.
        b_tmp = cb%alpha/255.

         co%red =  clampInt(int(ca%red * a_tmp) + int(cb%red * (1. - a_tmp)), 0, 255)
         co%green =  clampInt(int(ca%green * a_tmp) + int(cb%green * (1. - a_tmp)), 0, 255)
         co%blue =  clampInt(int(ca%blue * a_tmp) + int(cb%blue * (1. - a_tmp)), 0, 255)
         co%alpha = clampInt(int((b_tmp + (1. - b_tmp) * a_tmp)*255.), 0, 255)

   end function alpha_comp


   type(point) function pointSub(a, b)

      implicit none

      type(point), intent(IN) :: a, b

      pointSub = point(a%x - b%x, a%y - b%y)

   end function pointSub


   type(point) function pointAdd(a, b)

      implicit none

      type(point), intent(IN) :: a, b

      pointAdd = point(a%x + b%x, a%y + b%y)

   end function pointAdd


   type(point) function pointmultscal(a, b)

      implicit none

      type(point), intent(IN) :: a
      real,        intent(IN) :: b

      pointmultscal = point(int(a%x * b), int(a%y * b))

   end function pointmultscal
end module Draw