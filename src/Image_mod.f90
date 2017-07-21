Module Image

   implicit none

   type RGBimage
      integer, dimension(:,:), pointer :: Red, Green, Blue
      integer                          :: width, height
   end type RGBimage

   type RGBAimage
      integer, dimension(:,:), pointer :: Red, Green, Blue, Alpha
      integer                          :: width, height
   end type RGBAimage

   type RGB
      integer :: Red, Green, Blue
   end type RGB

   type RGBA
      integer :: Red, Green, Blue, Alpha
   end type RGBA
   
   interface alloc_image
      module procedure alloc_imageRGB
      module procedure alloc_imageRGBA
   end interface 

   interface init_image
      module procedure init_imageRGB
      module procedure init_imageRGBA
   end interface 

   interface get_pixel 
      module procedure get_pixelRGB
      module procedure get_pixelRGBA
   end interface

   interface set_pixel
      module procedure set_pixelRGB
      module procedure set_pixelRGBA
   end interface

   interface fill_img
      module procedure fill_imgRGB
      module procedure fill_imgRGBA
   end interface

   interface flip
      module procedure flip_RGBA
      ! module procedure flip_RGB
   end interface

   interface read_ppm
      module procedure read_ppm_RGB
      module procedure read_ppm_RGBA
   end interface read_ppm

   interface write_ppm
      module procedure write_ppm_RGB
      module procedure write_ppm_RGBA
   end interface

   interface open_image
      module procedure open_imageRGB
      module procedure open_imageRGBA
   end interface

   interface save_image
      module procedure save_imageRGB
      module procedure save_imageRGBA
   end interface

   interface operator (==)
      module procedure RGBequal
      module procedure RGBAequal
   end interface
   
   interface operator (/=)
      module procedure RGBnotequal
      module procedure RGBAnotequal
   end interface
   
   interface operator (.dist.)
      module procedure colourdistance
   end interface

   interface operator (*)
      module procedure colourmultiplyA
      module procedure colourmultiply
      module procedure colourmultiplycolour
   end interface

   interface operator (+)
      module procedure colouradd
      module procedure colouraddA
   end interface

   interface assignment (=)
      module procedure RGBimageequal
      module procedure RGBAimageequal
   end interface
   
   private
   public :: operator(+), assignment(=), operator(*), operator(.dist.), operator(==), operator(/=)
   public :: save_image, open_image, write_ppm, read_ppm, flip, clampInt
   public :: alloc_image, init_image, get_pixel, set_pixel, fill_img
   public :: rgb, rgba, RGBimage, RGBAimage

Contains

   subroutine init_imageRGB(img)

      implicit none
      
      type(RGBimage), intent(OUT) :: img

      nullify(img%Red)
      nullify(img%Green)
      nullify(img%Blue)
      img%width = 0
      img%height = 0

   end subroutine init_imageRGB

  subroutine init_imageRGBA(img)

      implicit none
      
      type(RGBAimage), intent(OUT) :: img

      nullify(img%Red)
      nullify(img%Green)
      nullify(img%Blue)
      nullify(img%Alpha)

      img%width = 0
      img%height = 0

   end subroutine init_imageRGBA


   subroutine alloc_imageRGB(image, w, h)

      implicit none
      
      type(RGBimage)      :: image
      integer, intent(IN) :: w, h
      
      allocate(image%Red(w, h))
      allocate(image%Green(w, h))
      allocate(image%Blue(w, h))
      image%width = w
      image%height = h

   end subroutine alloc_imageRGB


   subroutine alloc_imageRGBA(image, w, h)

      implicit none
      
      type(RGBAimage)     :: image
      integer, intent(IN) :: w, h
      
      allocate(image%Red(w, h))
      allocate(image%Green(w, h))
      allocate(image%Blue(w, h))
      allocate(image%Alpha(w, h))

      image%width = w
      image%height = h

   end subroutine alloc_imageRGBA

   subroutine set_pixelRGB(img, x, y, colour)
   
      implicit none
      
      type(RGBimage), intent(INOUT) :: img
      integer,        intent(in)    :: x, y
      type(RGB),      intent(in)    :: colour
   
      if(x > img%width .or. x < 1)then
         return
      elseif(y > img%height .or. y < 1)then
         return
      end if

      img%Red(x, y) = colour%Red
      img%Green(x, y) = colour%Green
      img%Blue(x, y) = colour%Blue
      
   end subroutine set_pixelRGB


   subroutine set_pixelRGBA(img, x, y, colour)
   
      implicit none
      
      type(RGBAimage), intent(INOUT) :: img
      integer,        intent(in)     :: x, y
      type(RGBA),      intent(in)    :: colour
   
      if(x > img%width .or. x < 1)then
         return
      elseif(y > img%height .or. y < 1)then
         return
      end if

      img%Red(x, y) = colour%Red
      img%Green(x, y) = colour%Green
      img%Blue(x, y) = colour%Blue
      img%Alpha(x, y) = colour%Alpha

      
   end subroutine set_pixelRGBA
   
   
   subroutine get_pixelRGB(img, x, y, colour)
   
      implicit none
      
      type(RGBimage), intent(IN)    :: img
      type(RGB),      intent(INOUT) :: colour
      integer,        intent(in)    :: x, y

      
      colour%Red = img%Red(x, y)
      colour%Green = img%Green(x, y)
      colour%Blue = img%Blue(x, y)
   
   end subroutine get_pixelRGB


   subroutine get_pixelRGBA(img, x, y, colour)
   
      implicit none
      
      type(RGBAimage), intent(IN)    :: img
      type(RGBA),      intent(INOUT) :: colour
      integer,        intent(in)     :: x, y

      
      colour%Red = img%Red(x, y)
      colour%Green = img%Green(x, y)
      colour%Blue = img%Blue(x, y)
      colour%Alpha = img%Blue(x, y)

   
   end subroutine get_pixelRGBA


   subroutine fill_imgRGB(img, colour)

      implicit none
      
      type(RGBimage), intent(INOUT) :: img
      type(RGB),      intent(IN)    :: colour
      integer                       :: i, j
      
      do j = 1, img%height
         do i = 1, img%width
            img%Red(i,j) = colour%Red
            img%Green(i,j) = colour%Green
            img%Blue(i,j) = colour%Blue
         end do
      end do
   end subroutine fill_imgRGB

   subroutine fill_imgRGBA(img, colour)

      implicit none
      
      type(RGBAimage), intent(INOUT) :: img
      type(RGBA),      intent(IN)    :: colour
      integer                        :: i, j
      
      do j = 1, img%height
         do i = 1, img%width
            img%Red(i,j) = colour%Red
            img%Green(i,j) = colour%Green
            img%Blue(i,j) = colour%Blue
            img%Alpha(i,j) = colour%Alpha
         end do
      end do
   end subroutine fill_imgRGBA
   
   subroutine RGBimageequal(a1, a2)

      implicit none

      type(RGBimage), intent(OUT) :: a1
      type(RGBimage), intent(IN)  :: a2

      a1%red = a2%red
      a1%green = a2%green
      a1%blue = a2%blue

   end subroutine RGBimageequal
   
   
   subroutine RGBAimageequal(a1, a2)

      implicit none

      type(RGBAimage), intent(OUT) :: a1
      type(RGBAimage), intent(IN)  :: a2

      a1%red = a2%red
      a1%green = a2%green
      a1%blue = a2%blue
      a1%alpha = a2%alpha
      
   end subroutine RGBAimageequal


   logical function RGBequal(c1, c2)
   
      type(RGB), intent(IN) :: c1, c2
      
      RGBequal = .TRUE.
      if( (c1%red == c2%red) .and. (c1%Green == c2%Green) .and. (c1%Blue == c2%Blue))then
         return
      end if
      RGBequal = .FALSE.
      
   end function RGBequal

   logical function RGBAequal(c1, c2)
   
      type(RGBA), intent(IN) :: c1, c2
      
      RGBAequal = .TRUE.
      if( (c1%red == c2%red) .and. (c1%Green == c2%Green) .and. (c1%Blue == c2%Blue) .and. (c1%alpha == c2%alpha))then
         return
      end if
      RGBAequal = .FALSE.
      
   end function RGBAequal
   
   integer function clampInt(val, lo, hi)

      integer, intent(IN) :: lo, hi, val

      if(val < lo)then
         clampInt = lo
      elseif(val > hi)then
         clampInt = hi
      else
         clampInt = val
      end if

   end function clampInt


   logical function RGBnotequal(c1, c2)
   
      type(RGB), intent(IN) :: c1, c2
      
      RGBnotequal = .False.
      if( (c1%red == c2%red) .and. (c1%Green == c2%Green) .and. (c1%Blue == c2%Blue))then
         return
      end if
      RGBnotequal = .True.
      
   end function RGBnotequal


   logical function RGBAnotequal(c1, c2)
   
      type(RGBA), intent(IN) :: c1, c2
      
      RGBAnotequal = .False.
      if( (c1%red == c2%red) .and. (c1%Green == c2%Green) .and. (c1%Blue == c2%Blue) .and. (c1%alpha == c2%alpha))then
         return
      end if
      RGBAnotequal = .True.
      
   end function RGBAnotequal
   
   
   real function colourdistance(c1, c2)
      
         type(RGB), intent(IN) :: c1, c2
         
         colourdistance = sqrt(real(c1%red - c2%red)**2 + real(c1%Green - c2%Green)**2 &
                               + real(c1%Blue - c2%Blue)**2 )
                               
   end function colourdistance

   function colouraddA(c1, c2)

      type(RGBA), intent(IN) :: c1, c2
      type(RGBA)             :: colouraddA

      colouraddA = RGBA(c1%red + c2%red, c1%green + c2%green, c1%blue + c2%blue, c1%alpha + c2%alpha)

   end function colouraddA

   function colouradd(c1, c2)

      type(RGB), intent(IN) :: c1, c2
      type(RGB)             :: colouradd

      colouradd = RGB(c1%red + c2%red, c1%green + c2%green, c1%blue + c2%blue)

   end function colouradd

   function colourmultiplyA(colour, scalar)

      type(RGBA),intent(IN) :: colour
      type(RGBA)            :: colourmultiplyA
      real,      intent(IN) :: scalar

      colourmultiplyA = RGBA(int(colour%red*scalar), int(colour%green*scalar), int(colour%blue*scalar), colour%alpha)

   end function colourmultiplyA

   function colourmultiply(colour, scalar)

      type(RGB),intent(IN) :: colour
      type(RGB)            :: colourmultiply
      real,      intent(IN) :: scalar

      colourmultiply = RGB(int(colour%red*scalar), int(colour%green*scalar), int(colour%blue*scalar))

   end function colourmultiply


   function colourmultiplycolour(a, b)

      type(RGB),intent(IN) :: a, b
      type(RGB)            :: colourmultiplycolour

      colourmultiplycolour = RGB(int(a%red*b%red), int(a%green*b%green), int(a%blue*b%blue))

   end function colourmultiplycolour
   

   subroutine read_ppm_RGB(filename, img)
   
      implicit none
      
      type(RGBImage),     intent(OUT) :: img
      integer                         :: nmax, i, j, offset
      character(len = *), intent(IN)  :: filename
      character(2)                    :: mode
      character                       :: code
      
      img%width = 0
      img%height = 0
      nullify(img%Red)
      nullify(img%Green)
      nullify(img%Blue)
      
      open(56, file = filename, access='stream',form='formatted',status= 'old')
      
      read(56, '(A2)') mode
      read(56, *) img%width, img%height
      read(56, *) nmax
      inquire(56,pos=offset)
      close(56)
      open(56 ,file=filename,access='stream',status='old')
      read(56, pos=offset-1) code
      call alloc_image(img, img%width, img%height)

      if(mode == 'P6')then
         do j = 1, img%height
            do i = 1, img%width
               read(56) code
               img%Red(i,j) = iachar(code)
               read(56) code
               img%Green(i,j) = iachar(code)
               read(56) code
               img%Blue(i,j) = iachar(code)
            end do
         end do
      else
         print*,'Mode not supported!'
         stop
!         do j = 1, img%height
!            do i = 1, img%width
!               read(56) img%red(i,j)
!               read(56),img%green(i,j)
!               read(56),img%blue(i,j)
!               print*,img%red(i,j),img%green(i,j),img%blue(i,j)
!               call exit(0)
!            end do
!         end do
      end if

      close(56)
   
   end subroutine read_ppm_RGB

   subroutine read_ppm_RGBA(filename, img)
   
      implicit none
      
      type(RGBAImage),     intent(OUT) :: img
      integer                          :: nmax, i, j, offset, io
      character(len = *), intent(IN)   :: filename
      character(2)                     :: mode
      character                        :: code
      
!horrible code fix!!!!!!!!!!!!!!!

      img%width = 0
      img%height = 0
      nullify(img%Red)
      nullify(img%Green)
      nullify(img%Blue)
      nullify(img%Alpha)
      
      open(56, file = filename, access='stream',form='formatted',status= 'old', iostat=io)
      if(io /= 0)then
         print*,'Cant open'//trim(filename)
      else
         read(56, '(A2)') mode
         read(56, *) img%width, img%height
         read(56, *) nmax
         inquire(56,pos=offset)
         close(56)
         open(56 ,file=filename,access='stream',status='old')
         read(56, pos=offset-1) code
         call alloc_image(img, img%width, img%height)

         if(mode == 'P6')then
            do j = 1, img%height
               do i = 1, img%width
                  read(56) code
                  img%Red(i,j) = iachar(code)
                  read(56) code
                  img%Green(i,j) = iachar(code)
                  read(56) code
                  img%Blue(i,j) = iachar(code)
               end do
            end do
         else
            print*,'Mode not supported!'
            stop
   !         do j = 1, img%height
   !            do i = 1, img%width
   !               read(56) img%red(i,j)
   !               read(56),img%green(i,j)
   !               read(56),img%blue(i,j)
   !               print*,img%red(i,j),img%green(i,j),img%blue(i,j)
   !               call exit(0)
   !            end do
   !         end do
         end if
      end if
      close(56)
   
   end subroutine read_ppm_RGBA
   

   subroutine flip_RGBA(img)

      implicit none

      type(RGBAimage), intent(INOUT) :: img
      type(RGBAimage) :: tmp

      integer :: i, j,k

      call init_image(tmp)
      call alloc_image(tmp, img%width, img%height)
      k = 1
      do j = img%height,1, -1
         do i = 1, img%width
            tmp%red(i, k) = img%red(i, j)
            tmp%green(i, k) = img%green(i, j)
            tmp%blue(i, k) = img%blue(i, j)
            tmp%alpha(i, k) = img%alpha(i, j)
         end do
      k = k + 1
      end do

      img = tmp

   end subroutine flip_RGBA

   
   subroutine write_ppm_RGB(filename, img, mode)
   
      implicit none
      
      type(RGBimage),             intent(in) :: img
      character(len=*),           intent(in) :: filename
      character(len=*), optional, intent(in) :: mode
      integer                                :: i, j, u
      logical                                :: flag
      
      if(present(mode))then
         if(mode == 'P6')then
            flag = .TRUE.
         else
            flag=.FALSE.
         end if
      else
         flag = .FALSE.
      end if
      
      open(newunit=u, file = filename)
      
      if(flag)then
         write(u, '(A2)') 'P6'
      else
         write(u, '(A2)') mode
         print*,mode
      end if
      
      write(u, '(i0, " ",i0)') img%width, img%height
      write(u, '(i0)') 255

      if(flag)then
         do j =  img%height,1, -1
            do i = 1, img%width
               write(u, '(3A1)', advance='no') achar(img%Red(i,j)), achar(img%Green(i,j)), achar(img%Blue(i,j))
            end do
         end do
      else
         do j = 1, img%height
            do i = 1, img%width
               write(u, '(3(I3.1,1X))',advance='no') img%Red(i,j), img%Green(i,j), img%Blue(i,j)
            end do
         end do
      end if
      close(u)
      
   end subroutine write_ppm_RGB


   subroutine write_ppm_RGBA(filename, img, mode)
   
      implicit none
      
      type(RGBAImage),             intent(in) :: img
      character(len=*),           intent(in)  :: filename
      character(len=*), optional, intent(in)  :: mode
      integer                                 :: i, j
      logical                                 :: flag
      
      if(present(mode))then
         if(mode == 'P6')then
            flag = .TRUE.
         else
            flag = .FALSE.
         end if
      else
         flag = .FALSE.
      end if
      
      open(45, file = filename)
      
      if(flag)then
         write(45, '(A2)') 'P6'
      else
         write(45, '(A2)') mode
         print*,mode
      end if
      
      write(45, '(i0, " ",i0)') img%width, img%height
      write(45, '(i0)') 255

      if(flag)then
         do j = 1, img%height
            do i = 1, img%width
               write(45, '(3A1)', advance='no') achar(img%Red(i,j)), achar(img%Green(i,j)), achar(img%Blue(i,j))
            end do
         end do
      else
         do j = 1, img%height
            do i = 1, img%width
               write(45, '(3(I3.1,1X))',advance='no') img%Red(i,j), img%Green(i,j), img%Blue(i,j)
            end do
         end do
      end if
      close(45)
      
   end subroutine write_ppm_RGBA


   subroutine open_imageRGB(img, filename, format)
   
      implicit none
      
      type(RGBimage), intent(OUT) :: img
      character(*),   intent(IN)  :: filename, format
      
      call execute_command_line('convert '//filename//format//' '//filename//'.ppm')
      call read_ppm(filename//'.ppm', img)
      call execute_command_line('rm '//filename//'.ppm')
   
   end subroutine open_imageRGB


   subroutine open_imageRGBA(img, filename, format)
   
      implicit none
      
      type(RGBAimage), intent(OUT) :: img
      character(*),   intent(IN)  :: filename, format
      
      call execute_command_line('convert '//filename//format//' '//filename//'.ppm')
      call read_ppm(filename//'.ppm', img)
      call execute_command_line('rm '//filename//'.ppm')
   
   end subroutine open_imageRGBA


   subroutine save_imageRGB(img, filename, format)
   
      implicit none
      
      type(RGBimage), intent(IN) :: img
      character(*),   intent(IN) :: filename, format
      
      print*,'Saved image as: ',filename//format
      call write_ppm(filename//'.ppm', img, 'P6')
      call execute_command_line('convert '//filename//'.ppm '//filename//format)
      call execute_command_line('rm '//filename//'.ppm')
   
   end subroutine save_imageRGB


   subroutine save_imageRGBA(img, filename, format)
   
      implicit none
      
      type(RGBAimage), intent(IN) :: img
      character(*),   intent(IN) :: filename, format
      
      print*,'Saved image at: ',filename//format
      call write_ppm(filename//'.ppm', img, 'P6')
      call execute_command_line('convert '//filename//'.ppm '//filename//format)
      call execute_command_line('rm '//filename//'.ppm')
   
   end subroutine save_imageRGBA
end module image