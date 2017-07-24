module triangleclass

    use shapeclass
    use image

    implicit none

    type, extends(shape) :: triangle
        type(vector) :: vert(3), uvs(3), norms(3)
        contains
        
        ! procedure :: info => info_fn
    end type triangle

    ! private :: info_fn

    contains

    ! subroutine info_fn(this)

    !     class(triangle), intent(IN) :: this

    !     print*,'triangle', this%vert(:)

    ! end subroutine info_fn

end module triangleclass
