module triangleclass

    use shapeclass
    use image

    implicit none

    type, extends(shape) :: triangle
        type(vector) :: p1, p2, p3
        integer      :: obj

        contains
        
        procedure :: info => info_fn
    end type triangle

    private :: info_fn

    contains

    subroutine info_fn(this)

        class(triangle), intent(IN) :: this

        print*,'triangle', this%p1, this%p2, this%p3

    end subroutine info_fn

end module triangleclass
