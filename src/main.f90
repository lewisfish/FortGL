program openFl

    use Image,           only : save_image, flip, RGBAimage, RGBA
    use render
    use utils,           only : str

    use obj_reader
    use ply_reader
    use triangleclass
    use types

    implicit none

    !array of triangles
    type(triangle), allocatable :: tarray(:)

    type(RGBAimage)     :: img, zbuf, texture
    type(RGBA)          :: colour
    type(ivec)          :: screenCoor(3)
    type(vector)        :: worldCoor(3), v, light_dir, uv(3), centre, eye, norm(3)
    character(len=256)  :: arg, pwd
    integer             :: i, j, height, width, depth, idx, k
    real                :: intensity, finish, start
    real, allocatable   :: zbuffer(:)
    real                :: projection(4,4), viewport(4,4), modelview(4,4)


    call get_environment_variable('PWD',pwd)
    pwd = pwd(:len(trim(pwd))-3)

    !get file name
    call get_command_argument(1, arg)

    !set image size
    width = 800
    height = 800
    depth = 255

    light_dir = normal(vector(1.,-1.,1.))
    centre = vector(0., 0., 0.)
    eye = vector(1., 1., 3.)

    modelview = lookat(eye, centre, vector(0.,1.,0.))
    projection = proj(-1./magnitude(eye-centre))
    viewport = view_init(width/8, height/8, width*3/4, height*3/4, depth)

    !setup imgage object
    call init_image(img)
    call alloc_image(img, width, height)

    call init_image(zbuf)
    call alloc_image(zbuf, width, height)

    allocate(zbuffer(width*height))

    zbuffer = -huge(1.)

    !read mesh file and texture
    if(index(arg, '.ply') > 0)then
        call read_ply(trim(arg), tarray)
    else
        call read_obj(trim(arg), tarray, texture)
    end if
        
    !do render
    call cpu_time(start)
    do i = 1, size(tarray)
        do j = 1, 3
            v = tarray(i)%vert(j)
            screenCoor(j) = m2v(matmul(matmul(matmul(viewport,projection),modelview),v2m(v)))
            worldCoor(j) = v  
        end do

        !get uv coords
        do k = 1, 3
            uv(k) = tarray(i)%uvs(k)
            norm(k) = tarray(i)%norms(k)
        end do

        !adjust to size of texture
        uv(:)%x = uv(:)%x*texture%width
        uv(:)%y = uv(:)%y*texture%height
!                                              o       o       o    o      o      o
        !(img, pts, zbuffer, intensity, colour, texture, uvs, norms, light, wire)
        call draw_triangle(img, screenCoor(:), zbuffer(:), intensity,wire=.true.)! uvs=uv, norms=norm, light=light_dir, texture=texture)
    end do

    print*,
    call cpu_time(finish)
    print*,"Render took: "//str(finish-start,5)//'s'
    print*,

    !flip image
    call flip(img)
    !save image
    call save_image(img, trim(pwd)//"data/output", '.png')

    !asynchronously display image if supported, if not do it synchronously 
    call execute_command_line("eog "//trim(pwd)//"data/output.png", wait=.false.)

    !debug zbuffer
    do i =1, width-1
        do j = 1, height-1
            idx = i+j*width
            if(zbuffer(idx) < 0)zbuffer(idx)=0
            colour = rgba(int(zbuffer(idx)), int(zbuffer(idx)), int(zbuffer(idx)), 255)
            call set_pixel(zbuf, i, j, colour)
        end do
    end do

    call flip(zbuf)
    call save_image(zbuf, trim(pwd)//"data/zbuffer", '.png')
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