Module shaderclass

    use types, only : vector

    implicit none

    type :: shader
        real :: varying_intensity(3)
        Contains

            procedure :: fragment => fragment_fn
            procedure :: vertex   => vertex_fn
    end type shader

    private :: fragment_fn, vertex_fn
    public :: shader

    Contains

    logical function fragment_fn(this, bar_c, colour)

        use image, only : RGBA, operator(*)
        use types, only: operator(.dot.)

        implicit none

        class(shader) :: this
        type(vector), intent(IN)  :: bar_c
        type(vector) :: tmp
        type(RGBA)  :: colour

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

        class(shader)              :: this
        type(triangle), intent(IN) :: vertex
        integer,        intent(IN) :: i, j
        type(vector),   intent(IN) :: light
        real :: gl_vertex(4,1), vertex_fn(4,1)

        this%varying_intensity(j) = max(0., (vertex%norms(j) .dot. light))
        gl_vertex = v2m(vertex%vert(j))
        vertex_fn = matmul(matmul(matmul(viewport,projection),modelview),gl_vertex)
    end function vertex_fn
end Module shaderclass