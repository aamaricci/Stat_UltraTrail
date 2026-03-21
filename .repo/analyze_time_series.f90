program analysis
  USE SCIFOR
  implicit none

  integer            :: i
  character(len=12 ) :: time
  integer            :: t0,t
  integer            :: pos,flen,unit,ounit
  character(len=100) :: ifile,ofile
  type(pdf_kernel) :: pdf
  real(8) :: a,b,sigma
  real(8),dimension(:),allocatable :: data

  
  call parse_cmd_variable(ifile,"iFILE",default="")
  call parse_cmd_variable(ofile,"oFILE",default="out.dat")
  if(trim(ifile)=="")stop "ERROR: file undefined"

  open(free_unit(unit),file=reg(ifile))
  open(free_unit(ounit),file=reg(ofile))

  !Read first time:
  read(unit,*)pos,time
  t0 = get_time(time)
  rewind(unit)

  flen = file_length(reg(ifile))
  allocate(data(flen))
  do i=1,flen
     read(unit,*)pos,time
     t = get_time(time)
     data(i)=(t-t0)/60
     write(ounit,*)pos,(pos-1)/dble(flen),data(i),t
  enddo
  close(unit)
  close(ounit)

  
  ! data = data/60              !returns value in minutes
  a = data(1)
  b = data(flen)
  call pdf_allocate(pdf,flen)
  call pdf_set_range(pdf,0d0,2*b)
  call pdf_sigma(pdf,data,sigma)
  call pdf_push_sigma(pdf,sigma)
  call pdf_accumulate(pdf,data)
  call pdf_print(pdf,"PDF_"//str(ofile))


  

contains

  function get_time(time) result(t)
    character(len=*) :: time
    integer          :: hpos,t
    integer          :: h,m,s
    !Get first h which can be 1 or 2 digits:
    hpos = scan(time,":")
    h = str2int(time(:hpos-1))
    m = str2int(time(hpos+1:hpos+2))
    s = str2int(time(hpos+4:hpos+5))
    t = s + m*60 + h*3600
  end function get_time




  function str2int(str) result(int)
    character(len=*) :: str
    integer          :: int
    read(str,*)  int
  end function str2int


end program analysis
