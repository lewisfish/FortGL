program openFl

    use Image
    use Draw
    use obj_reader
    use triangleclass

    implicit none

    !array of triangles
    type(triangle), allocatable :: tarray(:)

    type(RGBAimage)     :: img
    type(RGBA)          :: white, red, green
    type(point)         :: p1, p2, p3
    character(len=256)  :: arg
    integer             :: i

    call get_command_argument(1, arg)

    white = RGBA(255, 255, 255, 255)
    red =   RGBA(255, 0,   0,   255)
    green = RGBA(0,   255, 0,   255)

    call init_image(img)
    call alloc_image(img, 800, 800)

    call read_obj(trim(arg), tarray)

    do i = 1, size(tarray)

        !convert triangle to 2d points
        p1 = point((tarray(i)%p1%x+1.)*800/2., (tarray(i)%p1%y+1.)*800/2.)
        p2 = point((tarray(i)%p2%x+1.)*800/2., (tarray(i)%p2%y+1.)*800/2.)
        p3 = point((tarray(i)%p3%x+1.)*800/2., (tarray(i)%p3%y+1.)*800/2.)

        !draw triangle with random colour
        call draw_triangle(img, p1,p2,p3, RGBA(mod(irand(),255),mod(irand(),255),mod(irand(),255),255))

    end do
    call flip(img)
    call write_ppm("/home/lewis/programs/OpenFl/data/output.ppm", img, 'P6')

end program openFl

!teapot  
! p1 = point((tarray(i)%p1%x+100.)*8/2., (tarray(i)%p1%y+60.)*8/2.)
! p2 = point((tarray(i)%p2%x+100.)*8/2., (tarray(i)%p2%y+60.)*8/2.)
! p3 = point((tarray(i)%p3%x+100.)*8/2., (tarray(i)%p3%y+60.)*8/2.)

!gourd
! p1 = point((tarray(i)%p1%x+3.)*400/2., (tarray(i)%p1%y+2.)*400/2.)
! p2 = point((tarray(i)%p2%x+3.)*400/2., (tarray(i)%p2%y+2.)*400/2.)
! p3 = point((tarray(i)%p3%x+3.)*400/2., (tarray(i)%p3%y+2.)*400/2.)