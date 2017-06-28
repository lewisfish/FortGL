program openFl

    use Image
    use Draw
    use obj_reader
    use triangleclass

    implicit none
    
    type(RGBAimage) :: img
    type(RGBA) :: white, red, green
    type(triangle), allocatable :: tarray(:)
    integer :: i
    type(point) :: p1, p2, p3
    character(len=32) :: arg


    call get_command_argument(1, arg)


    white = RGBA(255, 255, 255, 255)
    red =   RGBA(255, 0,   0,   255)
    green = RGBA(0,   255, 0,   255)

    call init_image(img)
    call alloc_image(img, 800, 800)


    call read_obj(trim(arg), tarray)

    do i = 1, size(tarray)

        !gourd
        p1 = point((tarray(i)%p1%x+3.)*400/2., (tarray(i)%p1%y+2.)*400/2.)
        p2 = point((tarray(i)%p2%x+3.)*400/2., (tarray(i)%p2%y+2.)*400/2.)
        p3 = point((tarray(i)%p3%x+3.)*400/2., (tarray(i)%p3%y+2.)*400/2.)

            ! allocate(tarray(3))
            ! tarray(1) = triangle(rgb(255,0,0), vector(10,70,0), vector(50,160,0), vector(70,80,0), 1)
            ! tarray(2) = triangle(rgb(255,0,0), vector(180,50,0), vector(150,1,0), vector(70,180,0), 1)
            ! tarray(3) = triangle(rgb(255,0,0), vector(180,150,0), vector(120,160,0), vector(130,180,0), 1)


            ! call draw_triangle(img, tarray(i), red)
            ! call draw_triangle(img, tarray(2), white)
            ! call draw_triangle(img, tarray(3), green)
        call draw_line(img, p1, p2, white)
        call draw_line(img, p2, p3, white)
        call draw_line(img, p3, p1, white)
    end do
    call flip(img)
    call write_ppm("output.ppm", img, 'P6')

end program openFl

!teapot  
! p1 = point((tarray(i)%p1%x+100.)*8/2., (tarray(i)%p1%y+60.)*8/2.)
! p2 = point((tarray(i)%p2%x+100.)*8/2., (tarray(i)%p2%y+60.)*8/2.)
! p3 = point((tarray(i)%p3%x+100.)*8/2., (tarray(i)%p3%y+60.)*8/2.)

!gourd
! p1 = point((tarray(i)%p1%x+3.)*400/2., (tarray(i)%p1%y+2.)*400/2.)
! p2 = point((tarray(i)%p2%x+3.)*400/2., (tarray(i)%p2%y+2.)*400/2.)
! p3 = point((tarray(i)%p3%x+3.)*400/2., (tarray(i)%p3%y+2.)*400/2.)