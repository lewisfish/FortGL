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
      module procedure swap_point
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
                     call get_pixelRGBA(img, p%x+i, p%y+j, c2)
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

               call get_pixelRGBA(img, p%x+x, p%y+y, c2)
               c1 = alpha_comp(colour, c2)
               call set_pixel(img, p%x + x, p%y + y , c1)

               call get_pixelRGBA(img, p%x+y, p%y+x, c2)
               c1 = alpha_comp(colour, c2)
               call set_pixel(img, p%x + y, p%y + x , c1)

               call get_pixelRGBA(img, p%x-y, p%y+x, c2)
               c1 = alpha_comp(colour,c2)
               call set_pixel(img, p%x - y, p%y + x , c1)

               call get_pixelRGBA(img, p%x-x, p%y+y, c2)
               c1 = alpha_comp(colour, c2)
               call set_pixel(img, p%x - x, p%y + y , c1)

               call get_pixelRGBA(img, p%x-x, p%y-y, c2)
               c1 = alpha_comp(colour, c2)
               call set_pixel(img, p%x - x, p%y - y , c1)

               call get_pixelRGBA(img, p%x-y, p%y-x, c2)
               c1 = alpha_comp(colour, c2)
               call set_pixel(img, p%x - y, p%y - x , c1)

               call get_pixelRGBA(img, p%x+y, p%y-x, c2)
               c1 = alpha_comp(colour, c2)
               call set_pixel(img, p%x + y, p%y - x , c1)

               call get_pixelRGBA(img, p%x+x, p%y-y, c2)
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
                     call get_pixelRGBA(img, p%x+i, p%y+j, c2)
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
   
   subroutine draw_triangleRGBA(img, p1, p2, p3, colour)

      use triangleclass

      implicit none

      type(RGBAimage),      intent(INOUT) :: img
      type(RGBA), optional, intent(IN)    :: colour
      type(point),          intent(INOUT) :: p1, p2, p3


      type(point) :: a,b
      integer     :: totalheight, i, seg_height, j
      real        :: alpha, beta

      if(present(colour))then

         if(p1%y > p2%y)call swap(p1, p2)
         if(p1%y > p3%y)call swap(p1, p3)
         if(p2%y > p3%y)call swap(p2, p3)

         totalheight = p3%y - p1%y
         if(totalheight == 0)totalheight = 1

         do i = p1%y, p2%y

            seg_height = p2%y - p1%y + 1
            alpha = real(i - p1%y)/totalheight
            beta = real(i - p1%y)/seg_height
            a = p1 + (p3-p1)*alpha
            b = p1 + (p2-p1)*beta
            if(a%x > b%x)call swap(a, b)
            do j = a%x, b%x
               call set_pixel(img, j, i, colour)
            end do
         end do
         do i = p2%y, p3%y

            seg_height = p3%y - p2%y+1
            alpha = real(i - p1%y)/totalheight
            beta = real(i - p2%y)/seg_height
            a = p1 + (p3-p1)*alpha
            b = p2 + (p3-p2)*beta
            if(a%x > b%x)call swap(a, b)
            do j = a%x, b%x
               call set_pixel(img, j, i, colour)
            end do
         end do
      else
         call draw_line(img, p1, p2, RGBA(255,255,255,255))
         call draw_line(img, p2, p3, RGBA(255,255,255,255))
         call draw_line(img, p3, p1, RGBA(255,255,255,255))
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


   subroutine swap_point(a, b)
   
      implicit none
      
      type(point), intent(INOUT) :: a, b
      type(point)                :: tmp
      
      tmp = a
      a = b
      b = tmp
      
   end subroutine swap_point
   

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
      call get_pixelRGBA(img, x, y, c)
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