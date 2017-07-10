program openFl

    use Image
    use Draw
    use obj_reader
    use triangleclass
    use types
    use iso_fortran_env
    use utils, only : str

    implicit none

    !array of triangles
    type(triangle), allocatable :: tarray(:)

    type(RGBAimage)     :: img, zbuf, texture
    type(RGBA)          :: colour
    type(ivec)          :: screenCoor(3)
    type(vector)        :: worldCoor(3), v, n, light_dir, uv(3)
    character(len=256)  :: arg, pwd
    integer             :: i, j, height, width, depth, idx, k
    real                :: intensity, finish, start
    real, allocatable   :: zbuffer(:)


    call get_environment_variable('PWD',pwd)
    pwd = pwd(:len(trim(pwd))-3)

    !get file name
    call get_command_argument(1, arg)

    !set file size
    width = 800
    height = 800
    depth = 255

    light_dir = vector(0.,0.,-1.)

    !setup imgage object
    call init_image(img)
    call alloc_image(img, width, height)

    call init_image(zbuf)
    call alloc_image(zbuf, width, height)

    allocate(zbuffer(width*height))

    zbuffer = -huge(1.)

    !read obj file and texture
    call read_obj(trim(arg), tarray, texture)

    !do render

    call cpu_time(start)
    do i = 1, size(tarray)
        do j = 1, 3
            v = tarray(i)%vert(j)
            screenCoor(j) = ivec(int((v%x+1)*width/2.), int((v%y+1)*height/2.), int((v%z+1.)*depth/2.))
            worldCoor(j) = v  
        end do

        !do simple lighting
        n = (worldCoor(3) - worldCoor(1)) .cross. (worldCoor(2) - worldCoor(1))
        n = normal(n)
        intensity = n .dot. light_dir
        if(intensity > 0)then
            !get uv coords
            do k = 1, 3
                uv(k) = tarray(i)%uvs(k)
            end do

            !adjust to size of texture
            uv(:)%x = uv(:)%x*texture%width
            uv(:)%y = uv(:)%y*texture%height

            intensity = intensity
            if(intensity > 1.) intensity=1.
                              !(img, pts, zbuffer, intensity, colour, texture, uvs, wire)
            call draw_triangle(img, screenCoor(:), zbuffer(:), intensity, texture=texture, uvs=uv)
        end if
    end do

    print*,
    call cpu_time(finish)
    print*,"Render took: "//str(finish-start,5)//'s'
    print*,

    !flip image
    call flip(img)
    !save image
    call save_image(img, trim(pwd)//"data/output", '.png')

    call execute_command_line("eog "//trim(pwd)//"data/output.png")


    do i =1, width-1
        do j = 1, height-1
            idx = i+j*width
            if(zbuffer(idx) < 0)zbuffer(idx)=0
            colour = rgba(int(zbuffer(idx)), int(zbuffer(idx)), int(zbuffer(idx)), 255)
            call set_pixel(zbuf, i, j, colour)
        end do
    end do

    call flip(zbuf)
    call save_image(zbuf, "/home/lewis/programs/OpenFl/data/zbuffer", '.png')

end program openFl

!head
! p1 = point((tarray(i)%p1%x+1.)*800/2., (tarray(i)%p1%y+1.)*800/2.)        
! p2 = point((tarray(i)%p2%x+1.)*800/2., (tarray(i)%p2%y+1.)*800/2.)        
! p3 = point((tarray(i)%p3%x+1.)*800/2., (tarray(i)%p3%y+1.)*800/2.)        

!teapot  
! p1 = point((tarray(i)%p1%x+100.)*8/2., (tarray(i)%p1%y+60.)*8/2.)
! p2 = point((tarray(i)%p2%x+100.)*8/2., (tarray(i)%p2%y+60.)*8/2.)
! p3 = point((tarray(i)%p3%x+100.)*8/2., (tarray(i)%p3%y+60.)*8/2.)

!gourd
! p1 = point((tarray(i)%p1%x+3.)*400/2., (tarray(i)%p1%y+2.)*400/2.)
! p2 = point((tarray(i)%p2%x+3.)*400/2., (tarray(i)%p2%y+2.)*400/2.)
! p3 = point((tarray(i)%p3%x+3.)*400/2., (tarray(i)%p3%y+2.)*400/2.)