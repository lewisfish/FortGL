Module obj_reader

    use types
    use triangleclass

    implicit none
    
    public  :: read_obj
    private :: read_vert, read_faces, make_triangle

    Contains

        subroutine read_obj(filename, tarray)

            implicit none

            character(*),   intent(IN)    :: filename
            type(triangle),allocatable, intent(INOUT) :: tarray(:)

            type(vector), allocatable :: varray(:)
            type(ivec),   allocatable :: farray(:)
            integer :: u, io, verts, faces
            character(len=256) :: line
            print*,filename
            open(newunit=u,file=filename,iostat=io)
            if(io /= 0)stop "file not found"
            verts = 0
            faces = 0
            do
                read(u, '(a)',iostat=io) line
                if(io /=0)exit  !eof
                line = adjustl(line)
                if(line(1:1) == '#' .or. line(1:1) == ' ' .or. line(1:1) == 'g')cycle
                if(line(1:2) == 'v ')verts = verts + 1
                if(line(1:2) == 'f ')faces = faces + 1
            end do
            close(u)

            allocate(varray(verts), farray(faces))
            call read_vert(filename, varray)
            call read_faces(filename, farray)

            allocate(tarray(faces))
            call make_triangle(varray, farray, tarray)
            deallocate(varray,farray)
        end subroutine read_obj


        subroutine make_triangle(varray, farray, tarray)

            implicit none

            type(vector), intent(IN)      :: varray(:)
            type(ivec),   intent(IN)      :: farray(:)
            type(triangle), intent(INOUT) :: tarray(:)

            type(vector) :: tmp(3)
            integer :: i

            do i = 1, size(farray)
                tmp(:) = [varray(farray(i)%x), varray(farray(i)%y), varray(farray(i)%z)]
                tarray(i) = triangle(rgb(255,0,0), tmp)
                ! tarray(i) = triangle(rgb(255,0,0), varray(farray(i)%x), varray(farray(i)%y), varray(farray(i)%z))
            end do

        end subroutine make_triangle


        subroutine read_vert(filename, array)

            implicit none

            character(*), intent(IN) :: filename
            type(vector), intent(OUT) :: array(:)

            integer            :: i, u, pos, io
            character(len=256) :: line
            character(len=256) :: char

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


        subroutine read_faces(filename, array)

            implicit none

            character(*), intent(IN)   :: filename
            type(ivec),   intent(OUT)  :: array(:)

            integer            :: i, u, pos, pos2, io
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
                        line = adjustl(line(2:))
                        pos = index(trim(line(:)),'/')-1
                        char = trim(adjustl(line(:pos)))
                        read(char,'(I4.1)')array(i)%x


                        line = adjustl(line(pos+1:))
                        pos = index(trim(line(:)), ' ')
                        pos2 = index(trim(line(pos:)), '/')+pos-2
                        char = trim(adjustl(line(pos:pos2)))
                        read(char,'(I4.1)')array(i)%y

                        line = adjustl(line(pos2+1:))
                        pos = index(trim(line(:)), ' ')
                        pos2 = index(trim(line(pos:)), '/')+pos-2
                        char = trim(adjustl(line(pos:pos2)))
                        read(char,'(I4.1)')array(i)%z
                        i = i + 1
                    else
                        line = adjustl(line(2:))
                        pos = index(trim(line(:)), ' ')
                        char = trim(adjustl(line(:pos)))
                        read(char,'(I4.1)')array(i)%x

                        line = adjustl(line(pos:))
                        pos = index(trim(line(:)), ' ')
                        char = trim(adjustl(line(:pos)))
                        read(char, '(I4.1)')array(i)%y

                        line = adjustl(line(pos:))
                        char = trim(adjustl(line))
                        read(char, '(I4.1)')array(i)%z
                        i = i + 1
                    end if
                else
                    cycle
                end if
            end do
            close(u)
        end subroutine read_faces
end module obj_reader