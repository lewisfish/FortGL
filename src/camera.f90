module camera

    use types

    implicit none

    real :: projection(4,4), viewport(4,4), modelview(4,4)

    private
    public :: identity, m2v, v2m, lookat, view_init, proj
    public :: projection, viewport, modelview

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


    function proj(coeff)

        implicit none

        real:: coeff
        real :: proj(4,4)

        proj = identity(proj)
        proj(4,3) = coeff

    end function proj


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

        real :: lookat(4,4), minv(4,4)!,tr(4,4)
        type(vector), intent(IN)  :: eye, centre,  up

        type(vector) :: x, y, z
        real :: tmpx(3),tmpy(3),tmpz(3),tmpc(3)
        integer :: i

        z = normal(eye-centre)
        x = normal(up .cross. z)
        y = normal(z .cross. x)
        ! print*,'y',y

        tmpx(1:3) = [x%x,x%y,x%z] 
        tmpy(1:3) = [y%x,y%y,y%z] 
        tmpz(1:3) = [z%x,z%y,z%z] 
        tmpc(1:3) = [centre%x,centre%y,centre%z] 

        minv = identity(minv)
        ! tr = identity(tr)
        do i = 1, 3
            minv(1,i) = tmpx(i)![x%x,x%y,x%z]
            minv(2,i) = tmpy(i)![y%x,y%y,y%z]
            minv(3,i) = tmpz(i)![z%x,z%y,z%z]
            minv(i,4) = -tmpc(i)![-centre%x,-centre%y,-centre%z]
        end do
        lookat = minv
    end function lookat
end module camera