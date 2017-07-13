Module obj_reader

    use types
    use triangleclass

    implicit none
    
    public  :: read_obj
    private :: read_vert, read_faces, read_texture_coor, make_triangle

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


            allocate(tarray(faces))
            call make_triangle(varray, farray, tarray, textarray, narray)
            deallocate(varray,farray,textarray)

            if(texts > 0)then
                ! call read_ppm(filename(:len(filename)-4)//'_diffuse.ppm', texture)
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

            integer            :: i, u, pos, io
            character(len=256) :: line, char

            open(newunit=u, file=filename, iostat=io)

            i = 1
            do
                read(u, '(a)',iostat=io) line

                if(io /=0)exit  !eof
                line = adjustl(line)
                if(line(1:1) == '#' .or. line(1:1) == ' ' .or. line(1:1) == 'g')cycle

                if(line(1:2) == 'v ')then
                    line = adjustl(line(2:))
                    pos = index(trim(line(:)), ' ')
                    char = trim(adjustl(line(:pos)))
                    read(char,'(f100.8)')array(i)%x

                    line = adjustl(line(pos:))
                    pos = index(trim(line(:)), ' ')
                    char = trim(adjustl(line(:pos)))
                    read(char, '(F100.8)')array(i)%y

                    char = adjustl(line(pos:))
                    read(char, '(F100.8)')array(i)%z
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

            character(len=256) :: line, char
            integer :: u, io, i, pos

            open(newunit=u, file=filename, iostat=io)


            i = 1
            do
                read(u, '(a)', iostat=io) line

                if(io /= 0)exit
                line = adjustl(line)
                if(line(1:1) == '#' .or. line(1:1) == ' ' .or. line(1:1) == 'g')cycle

                if(line(1:2) == 'vt')then
                    line = adjustl(line(3:))
                    pos = index(trim(line(:)), ' ')
                    char = trim(adjustl(line(:pos)))
                    read(char,'(f100.8)')array(i)%x

                    line = adjustl(line(pos:))
                    pos = index(trim(line(:)), ' ')
                    char = trim(adjustl(line(:pos)))
                    read(char, '(F100.8)')array(i)%y

                    char = adjustl(line(pos:))
                    read(char, '(F100.8)')array(i)%z
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
                    line = line(5:)
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

            integer            :: i, u, pos, io
            character(len=256) :: line
            character(len=20)  :: char

            open(newunit=u, file=filename, iostat=io)

            i = 1
            do
                read(u, '(a)',iostat=io) line

                if(io /=0)exit  !eof
                line = adjustl(line)
                if(line(1:1) == '#' .or. line(1:1) == ' ' .or. line(1:1) == 'g')cycle


                if(line(1:2) == 'f ')then
                    if(scan(line, '/') > 0)then

                        do
                            pos = scan(line, '/')
                            if(pos == 0)exit
                            line(pos:pos) = ' '
                        end do
                        line = trim(line(3:))

                        read(line,*)array(i,1)%x, array(i,2)%x, array(i,3)%x, &
                                   array(i,1)%y, array(i,2)%y, array(i,3)%y, &
                                   array(i,1)%z, array(i,2)%z, array(i,3)%z

                        i = i + 1
                    else
                        line = adjustl(line(2:))
                        pos = index(trim(line(:)), ' ')
                        char = trim(adjustl(line(:pos)))
                        read(char,'(I4.1)')array(i,1)%x

                        line = adjustl(line(pos:))
                        pos = index(trim(line(:)), ' ')
                        char = trim(adjustl(line(:pos)))
                        read(char, '(I4.1)')array(i,1)%y

                        line = adjustl(line(pos:))
                        char = trim(adjustl(line))
                        read(char, '(I4.1)')array(i,1)%z
                        i = i + 1
                    end if
                else
                    cycle
                end if
            end do
            close(u)
        end subroutine read_faces
end module obj_reader