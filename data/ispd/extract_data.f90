!> This program extrac data from 
program extract_data
use iso_fortran_env
!implicit none
  integer :: n, m , i
  character(4):: year(1)
  character(2):: month(12)

  year = "2010"
  month= (/ "01" , "02" , "03" , "04" , "05" , "06" , "07" , "08" , "09" , "10" , "11" , "12" /)

!  call exit


!  do i = 1 ,  12  
    ! first decade what files to extract
!    call system ("ls "//year//"/"//year//month(i)//"????.txt  > file_list")
!    call selection ("file_list" , year//"/"//year//month(i)//".dat")
!    call station_list (year//"/"//year//month(i)//".dat" , year//"/"//year//month(i)//".sta" )
!  enddo


!    call system ('find -name "2?????.sta"  |sort > file_list')
!    call count_unique_stations ("file_list" , n)

  call system ('find -name "2?????.dat"  |sort > file_list')
  call make_hourly ("uniq_sites" , "file_list")

contains
real*8 function mjd  (date)
  implicit none
  integer ,intent(in) :: date (6)
  integer :: aux (6)
  integer :: i , k
  real*8 :: dayfrac

  aux=date
  if ( aux(2) .le.  2) then
      aux(1)  = date(1) -  1
      aux(2) = date(2) +  12
  endif
  i = aux(1)/100
  k = 2 - i + int(i/4);
  mjd = int(365.25 * aux(1) ) - 679006
  dayfrac =  aux (4) / 24. + date(5)/(24. * 60. ) + date (6)/(24. * 3600. ) 
  mjd = mjd + int(30.6001*( aux(2) + 1)) + date(3) + k + dayfrac


end function

subroutine make_hourly (filename ,datfilename)
character(100) :: dummy , line , compar , line2 , zmiana
character(*):: filename ,datfilename
integer :: data (6) , i , j
real*8 :: tmp
logical:: sta
  
  open (newunit = iunit , action = "read" , file =filename)

  open (newunit = ioutput4 , file = "hourly/sites.sta", access="append")

  do iii = 1 ,1 !todo
  sta=.false.
  read (iunit , '(a)', iostat =io_stat) line
  if (io_stat /= 0 ) exit
  compar = adjustl(trim(line))
  do 
    i = index(trim(compar)," ")
    j = index(trim(compar),"/")
    if (i.eq.0.and.j.eq.0) then
      exit
    else
      compar(i:i) = "_"
      compar(j:j) = "_"
    endif
  enddo
  print * ,trim(compar) , "<----"
  open (newunit = ioutput , file = "hourly/"//trim(compar)//".dat")
  open (newunit = ioutput2 , file = "hourly/"//trim(compar)//".inf")

  zmiana=""
    i_licz = 0
    open (newunit = iunit2 , action = "read" , file =datfilename)
    do    !ii = 1 ,1 !todo
      read (iunit2 , '(a)', iostat =io_stat) dummy
      if (io_stat /= 0 ) exit
      print * , trim(dummy)

      open (newunit = iunit3 , action = "read" , file =dummy)
      liczplik=0
      do 
        read (iunit3 , '(a)', iostat =io_stat) line2
        if (io_stat /= 0 ) exit
        liczplik=liczplik+1
        if (line2(50:94).eq.line) then
          if (.not.sta) then
            write (ioutput4, '(a45, 2x,a)' ) compar, line2(13:40)
            sta=.true.
          endif
          if (line2(13:40).ne.zmiana.and.liczplik.ne.1) then 
            write (ioutput2, '(a,10x,a)' ) trim(zmiana) , line2
          endif

          read (line2(1:4) , *) data(1)
          read (line2(5:6) , *) data(2)
          read (line2(7:8) , *) data(3)
          read (line2(9:10) , *) data(4)
          tmp =mjd(data)
          i_licz=i_licz+1
          write(ioutput, '(f15.5,5x,2(a,2x))') , tmp ,  line2(1:12) , line2(41:49)
          zmiana=line2(13:40)
        endif
      enddo
      close(iunit3)
      
    enddo
    close(iunit2)




  enddo
close(iunit)

end subroutine

subroutine station_list ( filein , fileout )
  character (*) :: filein , fileout
  character (200) :: dummy2 ,dummy
  character(200) :: sites (8600)
  logical :: exists


  open (newunit = iunit  , file = filein  , action = "read"  )
  open (newunit = iunit2 , file = fileout , action = "write" )

  ii=0

  do   !jj =1 ,5000
    read (iunit , '(a)' , iostat=io_stat ) dummy
    if (io_stat /= 0 ) exit
    exists=.true.

    do k = 1 , ii
      if (dummy(51:94) .eq. sites(k)(51:94)) then 
        
        ! check if coordinates are not changed
        if (dummy(15:33).ne.sites(k)(15:33)) then
          write (*,*)
          write (*,*) dummy  !(15:33)
          write (*,*) sites(k) !(15:33)
        endif

        exists=.false.
        exit
      endif
    enddo

    if (exists) then
      ii=ii+1
      sites(ii) = dummy
      write (iunit2, '(a)' ) dummy
    endif
  enddo
  

  close (iunit)
  close (iunit2)
  print * , ii , "---"

end subroutine
! choose only 181 - station observation file

subroutine selection (filelist , output)
  character (*) :: filelist , output
  integer :: io_stat
  character(255) :: dummy

  open ( newunit = iunit3, file = output   , action = "write" )
  open ( newunit = iunit , file = filelist , action = "read"  )
  do 
    read ( iunit , '(a)' , iostat = io_stat ) dummy
    if ( io_stat /=0 ) exit
    print * , trim (dummy)
    open (newunit = iunit2 , file = trim(dummy), action = "read" )
    do
      read (iunit2 , '(a)' , iostat = io_stat) dummy
      if (io_stat /= 0 ) exit
      if (dummy(21:23).eq."181" .and. dummy(50:53).eq."0.00" ) then
!        iunit3 = 6
        write (iunit3,'(3(a,1x))') dummy(1:12) , dummy(27:62), dummy(145:188)
      endif
    enddo
    close(iunit2)
  enddo
  close(iunit)
  close(iunit3)
end subroutine

subroutine count_unique_stations (filelist , n )
  character (*) :: filelist
  character (len=255) :: dummy 
  character(len=255) , allocatable , dimension(:) :: sites
  
  call count_stations (filelist , m)
  print * , filelist , m
  allocate (sites(m)) 

  open (newunit = iunit , file = filelist , action = "read")
  n = 0
  do 
    read (iunit , '(a)' , iostat = io_stat) dummy
    if (io_stat /= 0 ) exit
      call count_files ( dummy , jj )
!      print * , trim(dummy) , jj
      open (newunit = iunit2 , file = dummy , action = "read")
      do j = 1 , jj
        n=n+1
        read (iunit2 , '(49x,a45,1x)' , iostat = io_stat) sites (j)
        if (io_stat /= 0 ) exit
      enddo
 enddo
 close (iunit)

 call count_uniq ( sites , n , size (sites) )
 open (newunit = iunit , file = "uniq_sites" , action = "write")
 write(iunit, '(a)') (sites(i) , i =1 ,n)

end subroutine
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine count_uniq ( array , n  , i )
  character(len=255), dimension(i) :: array 
  character(len=255), dimension(:) , allocatable :: array2

  allocate (array2(8000))

  k = 1
  array2(1) = array(1)
  outer: do i=2,size(array)
     do j=1,k
        if (array2(j) == array(i)) then
!            Found a match so start looking again
           cycle outer
        end if
     end do
!      No match found so add it to the output
     k = k + 1
     array2(k) = array(i)
  end do outer
  n=k

end subroutine
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine count_files (filelist , n )
  character (*) :: filelist
  integer :: n , io_stat , iunit
  character (len=255) :: dummy

  open (newunit = iunit , file = filelist , action = "read")
  
  n = 0
  do 
    read (iunit , * , iostat = io_stat) dummy
    if (io_stat /= 0 ) exit
    n=n+1
  enddo
  close(iunit)
end subroutine
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine count_stations (filelist , i )
  character (*) :: filelist
  character (len=255) :: dummy , dummy2
  integer :: io_stat

  open (newunit = iunit , file = filelist , action = "read")
  i = 0
  do 
    read (iunit , '(a)' , iostat = io_stat) dummy
    if (io_stat /=0 ) exit
      open (newunit = iunit2 , file = dummy , action = "read")
      do
        read (iunit2 , '(a)' , iostat = io_stat) dummy2
        if (io_stat /= 0 ) exit
        i=i+1
      enddo
      close(iunit2)
 enddo
  close(iunit)

end subroutine


end program
