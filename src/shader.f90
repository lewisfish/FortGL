Module shaderclass

    use types, only : vector

    implicit none

    type, abstract :: shader

    type(vector) :: varying_intensity

    Contains

    procedure :: fragment => fragment_sub
    procedure :: vertex   => vertex_sub

    end type shader

    private :: fragment_sub, vertex_sub

    Contains

    subroutine fragment_sub(this, bar_c, colour, flag)

        use image, only : RGBA

        implicit none

        class(shader) :: this
        type(vector), intent(IN)  :: bar_c
        type(RGBA),   intent(IN)  :: colour
        logical,      intent(OUT) :: flag

        real :: intensity

        intensity = this%varying_intensity .dot. bar_c
        colour = RGBA(255,255,255,255) * intensity
        flag = .false.
        
    end subroutine fragment_sub


    subroutine vertex_sub(this, vertex, i, j, light)

        use triangleclass

        implicit none

        class(shader)       :: this
        type(triangle), intent(IN) :: vertex
        integer, intent(IN) :: i, j
        type(vector) :: light, gl_vertex

        this%varying_intensity%x = vertex%norms(j) .dot. light
        gl_vertex = v2m(vertex%vert)
        
        screenCoor = matmul(matmul(matmul(viewport,projection),modelview),gl_vertex)

    end subroutine vertex_sub


end Module shaderclass