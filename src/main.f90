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


    function vertex_fn(this, vertex, light)

        use triangleclass
        use camera, only : viewport, projection, modelview, v2m, m2v

        implicit none

        class(toon)                :: this
        type(triangle), intent(IN) :: vertex
        type(vector),   intent(IN) :: light
        
        real    :: gl_vertex(4,1), vertex_fn(4,3)
        integer :: j

        do j = 1, size(vertex%vert)
            this%varying_intensity(j) = max(0., (vertex%norms(j) .dot. light))
            gl_vertex = v2m(vertex%vert(j))
            gl_vertex = matmul(matmul(matmul(viewport,projection),modelview),gl_vertex)
            vertex_fn(:,j) = gl_vertex(:,1)
        end do
    end function vertex_fn

end module userdef


program openFl

    use Image,           only : save_image, flip, RGBAimage, RGBA, init_image, alloc_image, set_pixel,fill_img
    use render,          only : draw_triangle
    use utils,           only : str, replace
    use shaderclass,     only : gourand, wireframe, tmap
    use obj_reader,      only : read_obj
    use ply_reader,      only : read_ply
    use triangleclass,   only : triangle
    use camera
    use userdef, only : toon

    use types

    implicit none

    type :: model
        type(triangle), allocatable :: tarray(:)
        type(RGBAImage) :: texture
    end type

    type :: models
        type(model), allocatable :: container(:)
    end type

    !array of triangles
    type(models) :: meshes
    type(triangle), allocatable :: tarray(:)
    type(wireframe)                  :: ishader

    type(RGBAimage)     :: img, zbuf, texture
    type(vector)        :: light_dir, centre, eye

    character(len=256)  :: pwd
    character(len=256), allocatable :: arg(:)

    integer             :: i, height, width, depth, n, k, p
    real                :: finish, start, screenCoor(4,3)
    real, allocatable   :: zbuffer(:)

    call get_environment_variable('PWD',pwd)
    pwd = pwd(:len(trim(pwd))-3)

    !get file name
    n = command_argument_count()
    allocate(arg(n))
    allocate(meshes%container(n))

    do i = 1, n
        call get_command_argument(i, arg(i))
        !read mesh file and texture
        if(index(arg(n), '.ply') > 0)then
            call read_ply(trim(arg(i)), tarray)
        else
            call read_obj(trim(arg(i)), tarray, texture, .false.)
        end if
        allocate(meshes%container(i)%tarray(size(tarray)))
        meshes%container(i)%tarray(:) = tarray(:)
        meshes%container(i)%texture = texture
    end do

    !set image size
    width  = 1600
    height = 1600
    depth  = 255

    ishader%width = width
    ishader%height = height

    !setup imgage object
    call init_image(img)
    call alloc_image(img, width, height)

    call init_image(zbuf)
    call alloc_image(zbuf, width, height)

    allocate(zbuffer(width*height))

    do p = 1, 1
       call fill_img(img, RGBA(0,0,0,255))

        light_dir = normal(vector(1.,1.,1.))
        centre = vector(0., 0., 0.)
        eye = vector(0., -1., 5.)
        screenCoor = 0.

        modelview = lookat(eye, centre, vector(0.,1.,0.))
        projection = proj(-1./magnitude(eye-centre))
        viewport = view_init(width/8, height/8, width*3/4, height*3/4, depth)

        zbuffer = -huge(1.)
        call cpu_time(start)

        ! do render
        do k = 1, size(meshes%container)
            ! ishader%texture = meshes%container(k)%texture
            do i = 1, size(meshes%container(k)%tarray)
                screenCoor = ishader%vertex(meshes%container(k)%tarray(i), light_dir)
                call draw_triangle(img, ishader, zbuffer, screenCoor)
            end do
        end do
        call cpu_time(finish)

        !flip image
        call flip(img)
        !save image
        call save_image(img, trim(pwd)//"data/output"//str(p), '.png')

        !asynchronously display image if supported, if not do it synchronously 
        call execute_command_line("eog "//trim(pwd)//"data/output"//str(p)//".png",wait=.false.)
    end do

    print*,' '
    print*,"Render took: ",str(finish-start,5),'s'
    print*,''
end program openFl