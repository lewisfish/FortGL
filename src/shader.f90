Module shaderclass

    use types, only : vector
    use image, only : RGBAimage

    implicit none

    !abstract shader type
    type, abstract :: shader
        real :: varying_intensity(3)
        Contains
            procedure(generic_frag), deferred, pass :: fragment
            procedure(generic_vert), deferred, pass :: vertex
    end type shader


    abstract interface
        function generic_vert(this, vertex, light)
            use triangleclass
            use camera, only : viewport, projection, modelview, v2m, m2v
            import :: shader
            class(shader) :: this
            type(triangle), intent(IN) :: vertex
            type(vector),   intent(IN) :: light
            real :: generic_vert(4,3)

        end function generic_vert
    end interface


    abstract interface
        logical function generic_frag(this, bar_c, colour)
            use image, only : RGBA, operator(*)
            use types, only: operator(.dot.), vector
            import :: shader
            class(shader) :: this
            type(vector), intent(IN)    :: bar_c
            type(RGBA),   intent(INOUT) :: colour
        end function generic_frag
    end interface


    !default shader which is a gourand shader
    type, extends(shader) :: gourand
        Contains
            procedure, pass(this) :: fragment => fragment_gourand
            procedure, pass(this) :: vertex => vertex_gourand
    end type gourand
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


    !shader which is a texture mapped shader
    type, extends(shader) :: tmap
        type(vector)    :: varying_uv(3)
        type(RGBAimage) :: texture
        Contains
            procedure, pass(this) :: fragment => fragment_tmap
            procedure, pass(this) :: vertex => vertex_tmap
    end type tmap

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


    !shader which is a texture mapped shader
    type, extends(shader) :: wireframe
        real :: line_thickness = 0.0025
        type(vector) :: altitudes
        integer :: width, height
        Contains
            procedure, pass(this) :: fragment => fragment_wire
            procedure, pass(this) :: vertex => vertex_wire
    end type wireframe
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    private :: fragment_gourand, vertex_gourand, fragment_tmap, vertex_tmap, fragment_wire, vertex_wire
    public :: shader, gourand, tmap, wireframe

    Contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    logical function fragment_gourand(this, bar_c, colour)

        use image, only : RGBA, operator(*)
        use types, only: operator(.dot.)

        implicit none

        class(gourand) :: this
        type(vector), intent(IN)    :: bar_c
        type(RGBA),   intent(INOUT) :: colour

        type(vector) :: tmp
        real :: intensity

        tmp = vector(this%varying_intensity(1), this%varying_intensity(2), this%varying_intensity(3))

        intensity = abs(tmp .dot. bar_c)
        colour = RGBA(255,255,255,255) * intensity
        fragment_gourand = .false.
        
    end function fragment_gourand


    function vertex_gourand(this, vertex, light)

        use triangleclass
        use camera, only : viewport, projection, modelview, v2m, m2v

        implicit none

        class(gourand)             :: this
        type(triangle), intent(IN) :: vertex
        type(vector),   intent(IN) :: light
        real :: gl_vertex(4,1), vertex_gourand(4,3)
        integer :: j

        do j = 1, size(vertex%vert)
            this%varying_intensity(j) = max(0.01, (vertex%norms(j) .dot. light))
            gl_vertex = v2m(vertex%vert(j))
            gl_vertex = matmul(matmul(matmul(viewport,projection),modelview),gl_vertex)
            vertex_gourand(:, j) = gl_vertex(:,1)
        end do
    end function vertex_gourand


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    logical function fragment_tmap(this, bar_c, colour)

        use image, only : RGBA, operator(*), get_pixel
        use types, only: operator(.dot.), operator(*), operator(+)

        implicit none

        class(tmap) :: this
        type(vector), intent(IN)    :: bar_c
        type(RGBA),   intent(INOUT) :: colour

        type(vector) :: tmp, uv
        real :: intensity

        tmp = vector(this%varying_intensity(1), this%varying_intensity(2), this%varying_intensity(3))

        intensity = tmp .dot. bar_c
        uv = this%varying_uv(1)*bar_c%x + this%varying_uv(2)*bar_c%y + this%varying_uv(3)*bar_c%z
        call get_pixel(this%texture, int(uv%x*this%texture%width), int(uv%y*this%texture%height), colour)
        colour = colour * intensity
        fragment_tmap = .false.
        
    end function fragment_tmap


    function vertex_tmap(this, vertex, light)

        use triangleclass
        use camera, only : viewport, projection, modelview, v2m, m2v

        implicit none

        class(tmap)              :: this
        type(triangle), intent(IN) :: vertex
        type(vector),   intent(IN) :: light
        
        real    :: gl_vertex(4,1), vertex_tmap(4,3)
        integer :: j

        do j = 1, size(vertex%vert)
            this%varying_uv(j) = vertex%uvs(j)
            this%varying_intensity(j) = max(0., (vertex%norms(j) .dot. light))
            gl_vertex = v2m(vertex%vert(j))
            gl_vertex = matmul(matmul(matmul(viewport,projection),modelview),gl_vertex)
            vertex_tmap(:, j) = gl_vertex(:,1)
        end do
    end function vertex_tmap

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    logical function fragment_wire(this, bar_c, colour)

        use image, only : RGBA, operator(*), get_pixel, alpha_comp
        use types, only: operator(.dot.), operator(*), operator(+)

        implicit none

        class(wireframe) :: this
        type(vector), intent(IN)    :: bar_c
        type(RGBA),   intent(INOUT) :: colour

        type(vector) :: tmp
        real         :: eintensity, d, intensity

        tmp = bar_c%x * vector(this%altitudes%x,0.,0.) + bar_c%y * vector(0.,this%altitudes%y,0.) + &
              bar_c%z * vector(0.,0.,this%altitudes%z) 

        d = min(tmp%x,tmp%y,tmp%z)
        ! if(d > 2.)then
        !     tmp = vector(this%varying_intensity(1), this%varying_intensity(2), this%varying_intensity(3))
        !     intensity = abs(tmp .dot. bar_c)
        !     colour = RGBA(255,0,0,255) * intensity
        ! else
            eintensity = 2.**(-.1*d*d)
            colour =  RGBA(255,255,255,255) * eintensity + RGBA(255,0,0,255) * (1. - eintensity)
        ! end if
        
        fragment_wire = .false.

    end function fragment_wire


    function vertex_wire(this, vertex, light)

        use triangleclass
        use camera, only : viewport, projection, modelview, v2m, m2v

        implicit none

        class(wireframe)           :: this
        type(triangle), intent(IN) :: vertex
        type(vector),   intent(IN) :: light

        type(vector) :: p0,p1,p2,v0,v1,v2

        real    :: gl_vertex(4,1), vertex_wire(4,3), area
        integer :: j

        do j = 1, size(vertex%vert)
            this%varying_intensity(j) = max(0., (vertex%norms(j) .dot. light))
            gl_vertex = v2m(vertex%vert(j))
            gl_vertex = matmul(matmul(matmul(viewport,projection),modelview),gl_vertex)
            vertex_wire(:,j) = gl_vertex(:,1)
        end do

        p0%z = 0.
        p1%z = 0.
        p2%z = 0.

        p0%x = this%width *  vertex%vert(1)%x/vertex_wire(4,1)
        p0%y = this%height * vertex%vert(1)%y/vertex_wire(4,1)

        p1%x = this%width *  vertex%vert(2)%x/vertex_wire(4,2)
        p1%y = this%height * vertex%vert(2)%y/vertex_wire(4,2)

        p2%x = this%width *  vertex%vert(3)%x/vertex_wire(4,3)
        p2%y = this%height * vertex%vert(3)%y/vertex_wire(4,3)


        v0 = p2 - p1
        v1 = p2 - p0
        v2 = p1 - p0

        area = abs(v1%x*v2%y - v1%y*v2%x)

        this%altitudes = vector(area/magnitude(v0), area/magnitude(v1), area/magnitude(v2))

        ! this%altitudes = vector(altitude(vertex%vert(1),vertex%vert(2),vertex%vert(3)), &
        !                         altitude(vertex%vert(2),vertex%vert(3),vertex%vert(1)), &
        !                         altitude(vertex%vert(3),vertex%vert(1),vertex%vert(2)))
        ! print*,this%altitudes
        ! stop
    end function vertex_wire

    real function altitude(a, b, c)

        use types

        implicit none

        type(vector), intent(IN) :: a, b, c
        type(vector) :: ba, bc, ba_on_bc

        ba = a - b
        bc = c - b
        ba_on_bc = (ba .dot. bc) * bc
        altitude = magnitude(ba - ba_on_bc)

    end function altitude
end Module shaderclass