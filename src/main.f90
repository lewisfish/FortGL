program openFl

    use Image
    use Draw
    use obj_reader
    use ply_reader
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
    type(vector)        :: worldCoor(3), v, n, light_dir, uv(3), centre, eye, norm(3)
    character(len=256)  :: arg, pwd
    integer             :: i, j, height, width, depth, idx, k
    real                :: intensity, finish, start
    real, allocatable   :: zbuffer(:)
    real :: projection(4,4), viewport(4,4), modelview(4,4)


    call get_environment_variable('PWD',pwd)
    pwd = pwd(:len(trim(pwd))-3)

    !get file name
    call get_command_argument(1, arg)


    !set image size
    width = 800
    height = 800
    depth = 255

    light_dir = normal(vector(0.,0.,-1.))
    centre = vector(0., 0., 0.)
    eye = vector(0., 0., .1)

    modelview = lookat(eye, centre, vector(0.,1.,0.))
    projection = identity(projection)
    projection(4,3) = -1./magnitude(eye-centre)
    viewport = view_init(width/8, height/8, width*3/4, height*3/4, depth)

! print*,m2v(matmul(matmul(matmul(viewport,projection),modelview),v2m(v)))
! stop
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
            ! screenCoor(j) = m2v(matmul(matmul(matmul(viewport,projection),modelview),v2m(v)))
            ! print*,screenCoor
            ! stop
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
                norm(k) = tarray(i)%norms(k)
            end do

            !adjust to size of texture
            uv(:)%x = uv(:)%x*texture%width
            uv(:)%y = uv(:)%y*texture%height

!                                              o       o       o    o      o      o
            !(img, pts, zbuffer, intensity, colour, texture, uvs, norms, light, wire)
            call draw_triangle(img, screenCoor(:), zbuffer(:), intensity, wire=.true.)!uvs=uv, norms=norm, light=light_dir, texture=texture)
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

    !asynchronously display image if supported, if not do it synchronously 
    call execute_command_line("eog "//trim(pwd)//"data/output.png", wait=.false.)


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

contains

    type(ivec) function m2v(m)

        implicit none

        real :: m(:,:)

        m2v = ivec(int(m(1,1)/m(4,1)), int(m(2,1)/m(4,1)), int(m(3,1)/m(4,1)))

    end function m2v

    function v2m(v)

        implicit none

        real :: v2m(4,1)
        type(vector), intent(IN) :: v

        v2m(1,1) = v%x
        v2m(2,1) = v%y
        v2m(3,1) = v%z
        v2m(4,1) = 1.

    end function v2m


    function identity(m)

        implicit none

        real, intent(INOUT) :: m(:,:)
        real :: identity(size(m,1), size(m,2))

        integer :: i, j

        do i = 1, size(m,1)
            do j = 1, size(m,2)
                if(i == j)then
                    m(i, j) = 1.
                else
                    m(i, j) = 0.
                end if
            end do
        end do
        identity = m
    end function identity


    function view_init(x, y, w, h, depth)

        implicit none

        real :: view_init(4,4)
        integer, intent(IN)  :: x, y, w, h, depth

        view_init = identity(view_init)
        view_init(1,4) = x + w/2.
        view_init(2,4) = y + h/2.
        view_init(3,4) = depth/2.

        view_init(1,1) = w/2.
        view_init(2,2) = h/2.
        view_init(3,3) = depth/2.

    end function view_init

    function lookat(eye, centre, up)

        implicit none

        real :: lookat(4,4), minv(4,4),tr(4,4)
        type(vector), intent(IN)  :: eye, centre,  up

        type(vector) :: x, y, z

        z = normal(eye-centre)
        x = normal(up .cross. z)
        y = normal(z .cross. x)

        minv = identity(minv)
        tr = identity(tr)
            minv(1,1:3) = [x%x,x%y,x%z]
            minv(2,1:3) = [y%x,y%y,y%z]
            minv(3,1:3) = [z%x,z%y,z%z]
            tr(1:3,4) = [-centre%x,-centre%y,-centre%z]

        lookat = matmul(minv,tr)
    end function lookat


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