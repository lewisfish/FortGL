Module obj_reader

    use types
    use triangleclass

    implicit none
    
    public  :: read_obj
    private :: read_vert, read_faces, read_texture_coor, read_vert_normals, make_triangle

    Contains

        subroutine read_obj(filename, tarray, texture)

            use Image

            implicit none

            type(RGBAimage),             intent(INOUT) :: texture
            character(*),                intent(IN)    :: filename
            type(triangle), allocatable, intent(INOUT) :: tarray(:)

            type(vector), allocatable :: varray(:), textarray(:), narray(:)
            type(ivec),   allocatable :: farray(:,:)

            integer            :: u, io, verts, faces, texts, norms
            character(len=256) :: line

            print*,'Reading: ',filename
            open(newunit=u,file=filename,iostat=io)
            if(io /= 0)stop "file not found"
            verts = 0
            faces = 0
            texts = 0
            norms = 0
            do
                read(u, '(a)',iostat=io) line
                if(io /=0)exit  !eof
                line = adjustl(line)
                if(line(1:1) == '#' .or. line(1:1) == ' ' .or. line(1:1) == 'g')cycle
                if(line(1:2) == 'v ')verts = verts + 1
                if(line(1:2) == 'f ')faces = faces + 1
                if(line(1:2) == 'vt')texts = texts + 1
                if(line(1:2) == 'vn')norms = norms + 1
            end do
            close(u)

            allocate(varray(verts), farray(faces,3), textarray(texts), narray(norms))
            call read_vert(filename, varray)
            call read_faces(filename, farray)
            call read_texture_coor(filename, textarray)
            call read_vert_normals(filename, narray)

            !fit mesh into bi-unit cube
            ! call fit_mesh(varray)

            if(allocated(tarray))deallocate(tarray)
            allocate(tarray(faces))
            call make_triangle(varray, farray, tarray, textarray, narray)
            deallocate(varray,farray,textarray, narray)

            if(texts > 0)then
                call open_image(texture, filename(:len(filename)-4)//'_diffuse', '.tga')
                call flip(texture)
            end if
        end subroutine read_obj


        subroutine make_triangle(varray, farray, tarray, textarray, narray)

            implicit none

            type(vector), intent(IN)      :: varray(:), textarray(:), narray(:)
            type(ivec),   intent(IN)      :: farray(:,:)
            type(triangle), intent(INOUT) :: tarray(:)

            type(vector) :: tmp(3), tmp2(3), tmp3(3)
            integer :: i

            do i = 1, size(farray,1)
                tmp(:) = [varray(farray(i,1)%x), varray(farray(i,1)%y), varray(farray(i,1)%z)]
                if(size(textarray) > 1)then
                    tmp2(:) = [textarray(farray(i,2)%x), textarray(farray(i,2)%y), textarray(farray(i,2)%z)]
                end if
                if(size(narray) > 1)then
                    tmp3(:) = [narray(farray(i,3)%x), narray(farray(i,3)%y), narray(farray(i,3)%z)]
                end if
                tarray(i) = triangle(rgb(255,0,0), tmp, tmp2, tmp3)
            end do

        end subroutine make_triangle


        subroutine read_vert(filename, array)

            implicit none

            character(*), intent(IN) :: filename
            type(vector), intent(OUT) :: array(:)

            integer            :: i, u, io
            character(len=256) :: line

            open(newunit=u, file=filename, iostat=io)

            i = 1
            do
                read(u, '(a)',iostat=io) line

                if(io /=0)exit  !eof
                line = adjustl(line)
                if(line(1:1) == '#' .or. line(1:1) == ' ' .or. line(1:1) == 'g')cycle

                if(line(1:2) == 'v ')then

                    line = adjustl(line(2:))
                    read(line,*)array(i)%x, array(i)%y, array(i)%z

                    i = i + 1
                else
                    cycle
                end if
            end do
            close(u)
        end subroutine read_vert


        subroutine read_texture_coor(filename, array)

            implicit none

            character(*), intent(IN)    :: filename
            type(vector), intent(INOUT) :: array(:)

            character(len=256) :: line
            integer            :: u, io, i,c,pos

            open(newunit=u, file=filename, iostat=io)


            i = 1
            do
                read(u, '(a)', iostat=io) line

                if(io /= 0)exit
                line = adjustl(line)
                if(line(1:1) == '#' .or. line(1:1) == ' ' .or. line(1:1) == 'g')cycle

                if(line(1:2) == 'vt')then

                    line = adjustl(line(3:))
                    c = 0
                    do
                        pos = scan(trim(line), ' ')

                        if(pos > 0)c = c + 1
                        if(pos == 0)exit
                        line(pos:pos) = ','
                    end do
                 
                    if(c == 1)then
                        read(line,*)array(i)%x, array(i)%y
                        array(i)%z = 0.
                    elseif(c == 2)then 
                        read(line,*)array(i)%x, array(i)%y, array(i)%z
                    else
                        error stop 'Broken texture vert read...'
                    end if
                    i = i + 1
                else
                    cycle
                end if


            end do
            close(u)
        end subroutine read_texture_coor


        subroutine read_vert_normals(filename, array)

            implicit none

            character(*), intent(IN)  :: filename
            type(vector), intent(OUT) :: array(:)

            character(len=256) :: line
            integer :: u, io, i

            open(newunit=u, file=filename, iostat=io)

            i = 1
            do 
                read(u, '(a)', iostat=io)line
        
                if(io /= 0)exit  !eof

                line = adjustl(line)
                if(line(1:1) == '#' .or. line(1:1) == ' ' .or. line(1:1) == 'g')cycle

                if(line(1:2) == 'vn')then
                    line = adjustl(line(3:))
                    read(line, *)array(i)%x, array(i)%y, array(i)%z
                    i = i + 1
                end if
            end do
            close(u)
        end subroutine read_vert_normals


        subroutine read_faces(filename, array)

            implicit none

            character(*), intent(IN)   :: filename
            type(ivec),   intent(OUT)  :: array(:, :)

            integer            :: i, u, pos, io, posold,c
            character(len=256) :: line
            logical            :: flag

            open(newunit=u, file=filename, iostat=io)

            i = 1
            do
                read(u, '(a)',iostat=io) line

                if(io /=0)exit  !eof
                line = adjustl(line)
                if(line(1:1) == '#' .or. line(1:1) == ' ' .or. line(1:1) == 'g')cycle


                if(line(1:2) == 'f ')then
                    if(scan(line, '/') > 0)then
                        posold = 0
                        flag = .false.
                        c = 0
                        do
                            pos = scan(line, '/')
                            if(posold + 1 == pos)flag = .true.
                            posold = pos
                            if(pos == 0)exit
                            c = c + 1
                            line(pos:pos) = ' '
                        end do
                        line = trim(line(3:))
                        ! print*,c,trim(line)
                        ! stop
                        if(c == 6)then
                            if(flag)then
                                !case where format is #//# #//# #//#
                                read(line,*)array(i,1)%x, array(i,3)%x, &
                                            array(i,1)%y, array(i,3)%y, &
                                            array(i,1)%z, array(i,3)%z
                            else
                                !case where format is #/#/# #/#/# #/#/# 
                                read(line,*)array(i,1)%x, array(i,2)%x, array(i,3)%x, &
                                           array(i,1)%y, array(i,2)%y, array(i,3)%y, &
                                           array(i,1)%z, array(i,2)%z, array(i,3)%z
                            end if
                        elseif(c == 3)then
                            !case where format is #/# #/# #/# 
                            read(line,*)array(i,1)%x, array(i,2)%x, &
                                        array(i,1)%y, array(i,2)%y, &
                                        array(i,1)%z, array(i,2)%z
                        else
                            error stop 'unknown format for faces...'
                        end if
                        i = i + 1
                    else
                        !case where format is # # #
                        line = adjustl(line(2:))
                        read(line,*) array(i,1)%x, array(i,1)%y, array(i,1)%z

                        i = i + 1
                    end if
                else
                    cycle
                end if
            end do
            array(:,:)%x = abs(array(:,:)%x)
            array(:,:)%y = abs(array(:,:)%y)
            array(:,:)%z = abs(array(:,:)%z)

            close(u)
        end subroutine read_faces


        subroutine fit_mesh(array)

            implicit none

            type(vector), intent(INOUT) :: array(:)

            real :: xmax, ymax, zmax, xmin, ymin, zmin

            xmin = minval(array(:)%x)
            xmax = maxval(array(:)%x)

            ymin = minval(array(:)%y)
            ymax = maxval(array(:)%y)

            zmin = minval(array(:)%z)
            zmax = maxval(array(:)%z)

            array(:)%x = (2. * (array(:)%x - xmin)/ (xmax - xmin) -1.)
            array(:)%y = (2. * (array(:)%y - ymin)/ (ymax - ymin) -1.)
            array(:)%z = (2. * (array(:)%z - zmin)/ (zmax - zmin) -1.)

        end subroutine fit_mesh

end module obj_reader