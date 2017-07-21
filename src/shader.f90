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
        function generic_vert(this, vertex, i, j, light)
            use triangleclass
            use camera, only : viewport, projection, modelview, v2m, m2v
            import :: shader
            class(shader) :: this
            type(triangle), intent(IN) :: vertex
            integer,        intent(IN) :: i, j
            type(vector),   intent(IN) :: light
            real :: generic_vert(4,1)

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

    private :: fragment_gourand, vertex_gourand, fragment_tmap, vertex_tmap
    public :: shader, gourand, tmap

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

        intensity = tmp .dot. bar_c
        colour = RGBA(255,255,255,255) * intensity
        fragment_gourand = .false.
        
    end function fragment_gourand


    function vertex_gourand(this, vertex, i, j, light)

        use triangleclass
        use camera, only : viewport, projection, modelview, v2m, m2v

        implicit none

        class(gourand)              :: this
        type(triangle), intent(IN) :: vertex
        integer,        intent(IN) :: i, j
        type(vector),   intent(IN) :: light
        real :: gl_vertex(4,1), vertex_gourand(4,1)

        this%varying_intensity(j) = max(0., (vertex%norms(j) .dot. light))
        gl_vertex = v2m(vertex%vert(j))
        vertex_gourand = matmul(matmul(matmul(viewport,projection),modelview),gl_vertex)
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


    function vertex_tmap(this, vertex, i, j, light)

        use triangleclass
        use camera, only : viewport, projection, modelview, v2m, m2v

        implicit none

        class(tmap)              :: this
        type(triangle), intent(IN) :: vertex
        integer,        intent(IN) :: i, j
        type(vector),   intent(IN) :: light
        real :: gl_vertex(4,1), vertex_tmap(4,1)

        this%varying_uv(j) = vertex%uvs(j)

        this%varying_intensity(j) = max(0., (vertex%norms(j) .dot. light))
        gl_vertex = v2m(vertex%vert(j))
        vertex_tmap = matmul(matmul(matmul(viewport,projection),modelview),gl_vertex)
    end function vertex_tmap
end Module shaderclass