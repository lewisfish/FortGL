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


    ! subroutine draw_triangleRGBA(img, pts, zbuffer, intensity, colour, texture, uvs, norms, light, wire)
    subroutine draw_triangleRGBA(img, ishader, zbuffer, pts, wire)
      use triangleclass
      use shaderclass
      implicit none

      type(RGBAimage),           intent(INOUT) :: img
      type(shader) :: ishader
      ! type(RGBAimage), optional, intent(IN)    :: texture
      ! type(RGBA),      optional, intent(IN)    :: colour
      type(ivec),                intent(INOUT) :: pts(:)
      ! type(vector),    optional, intent(IN)    :: uvs(:)
      ! type(vector),    optional                :: norms(:), light
      logical,         optional                :: wire
      real,                      intent(INOUT) :: zbuffer(:)
      ! real,                      intent(INOUT) :: intensity

      ! type(vector) :: uv, n
      type(RGBA)   :: c

      type(vector) :: tmp
      type(ivec)   :: p
      integer      :: bmin(2), bmax(2), clamp(2), i, j, k
      real         :: bc_screen(3), w, z, frag_depth
      logical :: discard

      if(.not. present(wire))then
         bmin = [img%width, img%height-1]
         bmax = [0, 0]
         clamp = [img%width, img%height-1]

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

               tmp = barycentric(pts(1)/pts(1)%z, pts(2)/pts(2)%z, pts(3)/pts(3)%z, p)
               bc_screen = [tmp%x, tmp%y, tmp%z]
               z = pts(1)%y * tmp%x + pts(2)%y * tmp%y + pts(3)%y * tmp%z;
               w = pts(1)%z * tmp%x + pts(2)%z * tmp%y + pts(3)%z * tmp%z;
               frag_depth = max(0, min(255, int(z/w+.5)));

               if(tmp%x < 0. .or. tmp%y < 0. .or. tmp%z < 0. &
                  .or. zbuffer(int(p%x+p%y*img%width)) > frag_depth)cycle
                discard = ishader%fragment(tmp, c)
                if(.not. discard)then
                    zbuffer(int(p%x + p%y * img%width)) = p%z
                    call set_pixel(img, p%x, p%y, c)
                end if





               ! if(bc_screen(1) < 0. .or. bc_screen(2) < 0. .or. bc_screen(3) < 0.)cycle
               ! do k = 1, 3
               !    p%z = p%z + int(pts(k)%z*bc_screen(k))
               ! end do
               ! if(p%x < 1 .or. p%y < 1)cycle
               ! if(zbuffer(int(p%x + p%y * img%width)) < p%z)then
               !    zbuffer(int(p%x + p%y * img%width)) = p%z
               !    if(present(texture))then
               !       if(.not. present(uvs))error stop "Need uvs"
               !       !interpolate uv corrds
               !       uv = uvs(1)*tmp%x + uvs(2)*tmp%y + uvs(3)*tmp%z
               !       n = norms(1)*tmp%x + norms(2)*tmp%y + norms(3)*tmp%z
               !       n = normal(n)
               !       intensity = abs( n .dot. light)
               !       !get texture colour
               !       call get_pixel(texture, int(uv%x), int(uv%y), c)
               !       !add lighting
               !       ! c = rgbA(255,255,255,255)
               !       c = c * intensity
               !       call set_pixel(img, p%x, p%y, c)
               !    else
               !       !interpolate uv corrds
               !       n = norms(1)*tmp%x + norms(2)*tmp%y + norms(3)*tmp%z
               !       n = normal(n)
               !       intensity = abs( n .dot. light)
               !       c = rgbA(255,255,255,255)
               !       c = c * intensity
               !       call set_pixel(img, p%x, p%y, c)
               !    end if
               ! end if
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
end module render