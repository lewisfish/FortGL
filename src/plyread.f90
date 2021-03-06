Module ply_reader

    use types
    use triangleclass

    implicit none
    
    public  :: read_ply
    private :: read_vert, read_faces, make_triangle

    Contains

        subroutine read_ply(filename, tarray)

            use Image

            implicit none

            character(*),                intent(IN)    :: filename
            type(triangle), allocatable, intent(INOUT) :: tarray(:)

            type(vector), allocatable :: varray(:)
            type(ivec),   allocatable :: farray(:,:)

            integer            :: u, io, verts, faces, prop, lines

            print*,'Reading: ',filename
            open(newunit=u,file=filename,iostat=io)
            if(io /= 0)stop "file not found"
            verts = 0
            faces = 0
            close(u)

            call read_header(filename, verts, faces, prop, lines)

            print*,verts,faces, prop
            allocate(varray(verts), farray(faces,1))
            call read_vert(filename, varray, lines)
            call read_faces(filename, farray, lines, verts)

            allocate(tarray(faces))
            call make_triangle(varray, farray, tarray)
            deallocate(varray,farray)


        end subroutine read_ply


        subroutine read_header(filename, verts, faces, property, i)

            implicit none

            character(len=*), intent(IN)  :: filename
            integer,          intent(OUT) :: verts, faces, property

            integer            :: io, u, i
            character(len=256) :: line

            verts = 0
            faces = 0
            property = 0

            open(newunit=u,file=filename,iostat=io)

            if(io /= 0)stop "Can't open file in ply_reader"

            i = 1
            do
                read(u, '(a)',iostat=io) line

                if(io /= 0)exit  !eof
                line = adjustl(line)

                !check if ply file
                if(i == 1 .and. verify(adjustl(line(1:3)), 'ply') /= 0)stop 'not valid ply file'
                if(i == 2 .and. verify(adjustl(line(8:12)), 'ascii') /= 0)stop "Binary .ply files not supported"

                !ignore comments
                if(verify(line(1:7), "comment") == 0)then
                    i = i + 1
                    cycle
                end if

                !get # of faces
                if(verify(line(1:13), "element face") == 0)then
                    line = adjustl(line(13:))
                    read(line,*)faces
                end if

                !get # of verts
                if(verify(line(1:14), "element vertex") == 0)then
                    line = adjustl(line(15:))
                    read(line,*)verts
                end if

                !get # of properties
                if(verify(line(1:8), "property") == 0)then
                    line = adjustl(line(9:))

                    if(verify(line(1:4), "list") == 0)then
                        continue
                    else
                        property = property + 1
                    end if
                end if

                if(verify(line(1:10), "end_header") == 0)exit            
                i = i + 1

            end do
            close(u)
        end subroutine read_header


        subroutine make_triangle(varray, farray, tarray)

            implicit none

            type(vector), intent(IN)      :: varray(:)
            type(ivec),   intent(IN)      :: farray(:,:)
            type(triangle), intent(INOUT) :: tarray(:)

            type(vector) :: tmp(3), tmp2(3), tmp3(3)
            integer :: i

            do i = 1, size(farray,1)
                tmp(:) = [varray(farray(i,1)%x), varray(farray(i,1)%y), varray(farray(i,1)%z)]
                tarray(i) = triangle(rgb(255,0,0), tmp, tmp2, tmp3)
            end do

        end subroutine make_triangle


        subroutine read_vert(filename, array, lines)

            implicit none

            character(*), intent(IN)  :: filename
            type(vector), intent(OUT) :: array(:)

            integer            :: i, u, io,lines
            character(len=256) :: line
            real               :: valMax

            open(newunit=u, file=filename, iostat=io)

            read(u, '(a)',iostat=io) line
            if(io /= 0)stop  !eof

            line = adjustl(line)
            do i = 1, lines-1
                read(u, '(a)',iostat=io)line
            end do

            do i = 1, size(array)
                read(u, '(a)',iostat=io) line
                line = adjustl(line)
                read(line,*)array(i)%x, array(i)%y, array(i)%z
            end do
            close(u)

            valMax = max(maxval(abs(array%x)),maxval(abs(array%y)),maxval(abs(array%z)))
            if(valMax > 1.0)then
                array%x = array%x / valMax
                array%y = array%y / valMax
                array%z = array%z / valMax
            end if
        end subroutine read_vert


        subroutine read_faces(filename, array, lines, verts)

            implicit none

            character(*), intent(IN)   :: filename
            type(ivec),   intent(OUT)  :: array(:, :)
            integer :: lines, verts

            integer            :: i, u, io
            character(len=256) :: line

            open(newunit=u, file=filename, iostat=io)

            i = 1
            
            read(u, '(a)',iostat=io) line
            if(io /= 0)stop  !eof

            line = adjustl(line)
            do i = 1, lines+verts-1
                read(u, '(a)',iostat=io)line
            end do

            do i = 1, size(array)
                read(u, '(a)',iostat=io) line
                line = adjustl(line(2:))
                read(line,*)array(i,1)%x, array(i,1)%y, array(i,1)%z
                array(i,1)%x = array(i,1)%x + 1
                array(i,1)%y = array(i,1)%y + 1
                array(i,1)%z = array(i,1)%z + 1
            end do
            close(u)
        end subroutine read_faces
end module ply_reader