Module shaderclass

    use types, only : vector

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
            procedure, pass(this) :: fragment => fragment_fn
            procedure, pass(this) :: vertex => vertex_fn
    end type gourand


    private :: fragment_fn, vertex_fn
    public :: shader, gourand

    Contains

    logical function fragment_fn(this, bar_c, colour)

        use image, only : RGBA, operator(*)
        use types, only: operator(.dot.)

        implicit none

        class(gourand) :: this
        type(vector), intent(IN)  :: bar_c
        type(vector) :: tmp
        type(RGBA),   intent(INOUT)  :: colour

        real :: intensity

        tmp = vector(this%varying_intensity(1), this%varying_intensity(2), this%varying_intensity(3))

        intensity = tmp .dot. bar_c
        colour = RGBA(255,255,255,255) * intensity
        fragment_fn = .false.
        
    end function fragment_fn


    function vertex_fn(this, vertex, i, j, light)

        use triangleclass
        use camera, only : viewport, projection, modelview, v2m, m2v

        implicit none

        class(gourand)              :: this
        type(triangle), intent(IN) :: vertex
        integer,        intent(IN) :: i, j
        type(vector),   intent(IN) :: light
        real :: gl_vertex(4,1), vertex_fn(4,1)

        this%varying_intensity(j) = max(0., (vertex%norms(j) .dot. light))
        gl_vertex = v2m(vertex%vert(j))
        vertex_fn = matmul(matmul(matmul(viewport,projection),modelview),gl_vertex)
    end function vertex_fn
end Module shaderclass