module render

    use Image
    use types

    implicit none

    interface draw_line
        module procedure draw_lineRGB
        module procedure draw_lineRGBA
    end interface draw_line

    interface draw_triangle
        module procedure draw_triangleRGBA
    end interface
    
    private
    public :: draw_line, draw_triangle

contains

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
        type(point),          intent(IN)    :: p1, p2
        type(RGBA)                          :: c
        type(point)                         :: p1t, p2t
        integer                             :: x, y, dx, dy, error, ystep
        logical                             :: steep

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


    type(vector) function barycentric(a, b, c, p)

        use types

        implicit none

        type(vector) :: a, b, c, s(2), u, p

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


    subroutine draw_triangleRGBA(img, ishader, zbuffer, pts)

      use triangleclass
      use shaderclass

      implicit none

      type(RGBAimage), intent(INOUT) :: img
      real,            intent(INOUT) :: pts(:,:)
      real,            intent(INOUT) :: zbuffer(:)
      type(shader)                   :: ishader

      type(RGBA)   :: c
      type(vector) :: tmp, tmp1, tmp2, tmp3
      type(ivec)   :: p
      integer      :: i, j
      real         :: w, z, frag_depth, bmin(2), bmax(2),clamp(2)
      logical      :: discard

         bmin = [99999., 9999.]
         bmax = [-9999., -9999.]
         clamp = [img%width, img%height-1]

         !get bounding box for triangle
         do i = 1, 3
            do j = 1, 2
               bmin(j) = min(bmin(j), pts(j,i)/pts(4,i))
               bmax(j) = max(bmax(j), pts(j,i)/pts(4,i))
           end do
         end do

        do i = int(bmin(1)), int(bmax(1))
            do j = int(bmin(2)), int(bmax(2))
                p%x = i
                p%y = j
                p%z = 0

                pts(:,1) = pts(:,1)/pts(4,1)
                pts(:,2) = pts(:,2)/pts(4,2)
                pts(:,3) = pts(:,3)/pts(4,3)

                tmp1 = vector(pts(1,1), pts(2,1), pts(3,1))
                tmp2 = vector(pts(1,2), pts(2,2), pts(3,2))
                tmp3 = vector(pts(1,3), pts(2,3), pts(3,3))

                tmp = barycentric(tmp1, tmp2, tmp3, vector(p%x,p%y,p%z))

                z = pts(3,1)*tmp%x + pts(3,2)*tmp%y + pts(3,3)*tmp%z
                w = pts(4,1)*tmp%x + pts(4,2)*tmp%y + pts(4,3)*tmp%z
                frag_depth = max(0, min(255, int(z/w+.5)))
                if(tmp%x <0 .or. tmp%y <0 .or. tmp%z <0 .or. zbuffer(int(p%x + p%y * img%width)) > frag_depth)cycle
                discard = ishader%fragment(tmp, c)
                if(.not. discard)then
                  zbuffer(int(p%x + p%y * img%width)) = frag_depth
                    call set_pixel(img, p%x, p%y, c)
                end if


            end do
        end do

      ! else
      !    if(wire)then
            !wireframe render
            ! call draw_line(img, point(pts(1)%x, pts(1)%y), point(pts(2)%x, pts(2)%y), RGBA(255,255,255,255))
            ! call draw_line(img, point(pts(2)%x, pts(2)%y), point(pts(3)%x, pts(3)%y), RGBA(255,255,255,255))
            ! call draw_line(img, point(pts(3)%x, pts(3)%y), point(pts(1)%x, pts(1)%y), RGBA(255,255,255,255))

   end subroutine draw_triangleRGBA
end module render