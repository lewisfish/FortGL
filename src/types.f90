module types

    use Image

    implicit none


    type :: vector
        real :: x, y, z
    end type vector

    interface operator (.dot.)
        module procedure vecDot
    end interface

    interface operator (.cross.)
        module procedure vecCross
    end interface

    interface operator (-)
        module procedure vecSub
    end interface

    interface operator (*)
        module procedure vecMulA
        module procedure vecMulB
        module procedure colourmultiplyvector
    end interface

    interface operator (+)
        module procedure vecAdd
    end interface

    contains
        function colourmultiplyvector(b, a)

            type(RGB),    intent(IN) :: a
            type(vector), intent(IN) :: b
            type(RGB)                :: colourmultiplyvector

            colourmultiplyvector = RGB(int(a%red*b%x), int(a%green*b%y), int(a%blue*b%z))

        end function colourmultiplyvector


        type(vector) function vecMulA(a, b)

            implicit none

            type(vector), intent(IN) :: a
            real,         intent(IN) :: b

            vecMulA = vector(a%x * b, a%y * b, a%z * b)

        end function vecMulA


        type(vector) function vecMulB(a, b)

            implicit none

            type(vector), intent(IN) :: b
            real,         intent(IN) :: a

            vecMulB = vector(a * b%x, a * b%y, a * b%z)

        end function vecMulB


        type(vector) function vecSub(a, b)

            implicit none

            type(vector), intent(IN) :: a, b

            vecSub = vector(a%x - b%x, a%y - b%y , a%z - b%z)

        end function vecSub


        type(vector) function vecAdd(a, b)

            implicit none

            type(vector), intent(IN) :: a, b

            vecAdd = vector(a%x + b%x, a%y + b%y , a%z + b%z)

        end function vecAdd


        real function vecDot(a, b)

            implicit none

            type(vector), intent(IN) :: a, b

            vecDot = (a%x * b%x) + (a%y * b%y) + (a%z * b%z)

        end function vecDot


        type(vector) function vecCross(a, b)

            implicit none

            type(vector), intent(IN) :: a, b
            real                     :: i, j, k

            i = (a%y * b%z) - (b%y * a%z)
            j = (a%x * b%z) - (b%x * a%z)
            k = (a%x * b%y) - (b%x * a%y)

            vecCross = vector(i, j, k)

        end function vecCross


        real function magnitude(this)

            implicit none

            type(vector) :: this

            magnitude = sqrt(this%x**2 + this%y**2 + this%z**2)

        end function magnitude

        type(vector) function normal(this)

            implicit none

            type(vector), intent(in) :: this
            real :: mag

            mag = magnitude(this)

            normal = vector(this%x/mag, this%y/mag, this%z/mag)

        end function normal
end module types