module userdef

    use shaderclass

    implicit none

!user defined shaders here

    type, extends(shader) :: toon
        Contains
            procedure, pass(this) :: fragment => fragment_fn
            procedure, pass(this) :: vertex => vertex_fn
    end type toon

    Contains

    logical function fragment_fn(this, bar_c, colour)

        use image, only : RGBA, operator(*)
        use types, only: operator(.dot.)

        implicit none

        class(toon) :: this
        type(vector), intent(IN)  :: bar_c
        type(vector) :: tmp
        type(RGBA),   intent(INOUT)  :: colour

        real :: intensity

        tmp = vector(this%varying_intensity(1), this%varying_intensity(2), this%varying_intensity(3))

        intensity = tmp .dot. bar_c

        if(intensity >.85)then
            intensity = 1.
        elseif(intensity >.60)then
            intensity = .80
        elseif(intensity >.45) then
            intensity = .60
        elseif(intensity >.30)then
            intensity = .45
        elseif(intensity >.15)then
            intensity = .30
        else
            intensity = 0
        end if
            colour = RGBA(255,155,0,255) * intensity
        fragment_fn = .false.

end function fragment_fn


    function vertex_fn(this, vertex, i, j, light)

        use triangleclass
        use camera, only : viewport, projection, modelview, v2m, m2v

        implicit none

        class(toon)                :: this
        type(triangle), intent(IN) :: vertex
        integer,        intent(IN) :: i, j
        type(vector),   intent(IN) :: light
        
        real :: gl_vertex(4,1), vertex_fn(4,1)

        this%varying_intensity(j) = max(0., (vertex%norms(j) .dot. light))
        gl_vertex = v2m(vertex%vert(j))
        vertex_fn = matmul(matmul(matmul(viewport,projection),modelview),gl_vertex)
end function vertex_fn

end module userdef

program openFl

    use Image,           only : save_image, flip, RGBAimage, RGBA, init_image, alloc_image, set_pixel
    use render,          only : draw_triangle
    use utils,           only : str
    use shaderclass,     only : gourand
    use obj_reader,      only : read_obj
    use ply_reader,      only : read_ply
    use triangleclass,   only : triangle
    use camera
    use userdef, only : toon, tmap

    use types

    implicit none

    !array of triangles
    type(triangle), allocatable :: tarray(:)
    type(tmap)               :: ishader

    type(RGBAimage)     :: img, zbuf, texture
    type(RGBA)          :: colour
    type(vector)        :: light_dir, centre, eye
    character(len=256)  :: arg, pwd
    integer             :: i, j, height, width, depth, idx
    real                :: finish, start, screenCoor(4,3), tmp(4,1)
    real, allocatable   :: zbuffer(:)

    call get_environment_variable('PWD',pwd)
    pwd = pwd(:len(trim(pwd))-3)

    !get file name
    call get_command_argument(1, arg)

    !set image size
    width = 800
    height = 800
    depth = 255

    light_dir = normal(vector(1.,1.,1.))
    centre = vector(0., 0., 0.)
    eye = vector(0., 0., 3.)
    screenCoor = 0.

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
    
    ishader%texture = texture

    !do render
    call cpu_time(start)
    do i = 1, size(tarray)
        do j = 1, 3
           tmp= ishader%vertex(tarray(i), i, j, light_dir)
           screenCoor(:,j) = tmp(:,1)
        end do
        call draw_triangle(img, ishader, zbuffer, screenCoor)
    end do

    print*,' '
    call cpu_time(finish)
    print*,"Render took: ",str(finish-start,5),'s'
    print*,''

    !flip image
    call flip(img)
    !save image
    call save_image(img, trim(pwd)//"data/output", '.png')

    !asynchronously display image if supported, if not do it synchronously 
    call execute_command_line("eog "//trim(pwd)//"data/output.png")

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
