module DATA_TUPLE
  implicit none

  type tuple
     integer             :: index=0
     integer             :: rank
     real(8)             :: time
     character(len=3)    :: nat
     integer             :: age
     character(len=1)    :: mf
     real(8)             :: speed
     type(tuple),pointer :: next   =>null()
   contains
     procedure,pass      :: free     => free_tuple
     procedure,pass      :: read     => read_tuple
     procedure,pass      :: show     => show_tuple
  end type tuple

  interface tuple
     module procedure :: construct_tuple_data
  end interface tuple

  !EQUALITY operator A=B  [deep copy]
  interface assignment(=)
     module procedure :: equality_tuple
  end interface assignment(=)



  interface append
     module procedure :: append_I
     module procedure :: append_D
     module procedure :: append_C
  end interface append

  public :: tuple
  public :: assignment(=)
  public :: get_time

contains


  !+------------------------------------------------------------------+
  !PURPOSE:  Free a Tuple (destructor) 
  !+------------------------------------------------------------------+
  subroutine free_tuple(self)
    class(tuple),intent(inout) :: self
    self%rank    = 0
    self%time    = 0d0
    self%nat     = 'XXX'
    self%age     = 0
    self%mf      = ''
    self%speed   = 0d0
    !
    self%index   = 0
    self%next    => null()
  end subroutine free_tuple


  !+------------------------------------------------------------------+
  !PURPOSE:  Read a Tuple from a line in UNIT
  !+------------------------------------------------------------------+
  subroutine read_tuple(self,unit)
    class(tuple),intent(inout) :: self
    integer                    :: unit
    character(len=10)          :: time
    real(8)                    :: yob
    integer                    :: year
    integer                    :: values(8)
    character(len=10)          :: b(3)
    call date_and_time(b(1),b(2),b(3),values)
    year = values(1)
    read(unit,*)       &
         self%rank,    &
         time,         &
         self%nat,     &
         yob,          &
         self%mf,      &
         self%speed
    self%time=get_time(time)
    self%age = year - int(yob)
    !
    self%index = 0
    self%next  => null()
  end subroutine read_tuple


  !##################################################################
  !##################################################################
  !       CONSTRUCT TVECTOR FROM ARRAY 
  !##################################################################
  !##################################################################
  function construct_tuple_data(rank,time,nat,age,mf,speed) result(self)
    integer                            :: rank
    character(len=*)                   :: time
    character(len=3)                   :: nat
    integer                            :: age
    character(len=1)                   :: mf
    real(8)                            :: speed
    type(tuple),target                 :: self
    !
    call self%free()
    !
    self%rank    = rank
    self%time    = get_time(time)
    self%nat     = nat
    self%age     = age
    self%mf      = mf
    self%speed   = speed
    !
    self%index   = 0
    self%next    => null()
  end function construct_tuple_data



  !##################################################################
  !##################################################################
  !              OPERATIONS
  !##################################################################
  !##################################################################
  subroutine equality_tuple(a,b)
    type(tuple),intent(inout) :: a
    type(tuple),intent(in)    :: b
    integer                    :: i,N
    call a%free()
    a%rank    = b%rank
    a%time    = b%time
    a%nat     = b%nat
    a%age     = b%age
    a%mf      = b%mf
    a%speed   = b%speed
    a%index   =  b%index
    a%next    => b%next
  end subroutine equality_tuple


  !##################################################################
  !##################################################################
  !              SHOW TVECTOR
  !##################################################################
  !##################################################################
  subroutine show_tuple(self)
    class(tuple) :: self
    !
    write(*,"(A1)",advance='no')"["
    write(*,"(I6,1X)",advance='no')self%rank
    write(*,"(F18.2,1X)",advance='no')self%time
    write(*,"(A6,1X)",advance='no')self%nat
    write(*,"(I6,1X)",advance='no')self%age
    write(*,"(A6,1X)",advance='no')self%mf
    write(*,"(F6.2,1X)",advance='no')self%speed
    write(*,"(A1)",advance='no')"]"
    write(*,*)""
  end subroutine show_tuple




  !##################################################################
  !##################################################################
  !              AUXILIARY COMPUTATIONAL ROUTINES
  !##################################################################
  !##################################################################
  pure subroutine append_I(vec,val)
    integer,dimension(:),allocatable,intent(inout) :: vec
    integer,intent(in)                             :: val  
    integer,dimension(:),allocatable               :: tmp
    integer                                        :: n
    !
    if (allocated(vec)) then
       n = size(vec)
       allocate(tmp(n+1))
       tmp(:n) = vec
       call move_alloc(tmp,vec)
       n = n + 1
    else
       n = 1
       allocate(vec(n))
    end if
    !
    !Put val as last entry:
    vec(n) = val
    !
    if(allocated(tmp))deallocate(tmp)
  end subroutine append_I

  pure subroutine append_D(vec,val)
    real(8),dimension(:),allocatable,intent(inout) :: vec
    real(8),intent(in)                             :: val  
    real(8),dimension(:),allocatable               :: tmp
    integer                                        :: n
    !
    if (allocated(vec)) then
       n = size(vec)
       allocate(tmp(n+1))
       tmp(:n) = vec
       call move_alloc(tmp,vec)
       n = n + 1
    else
       n = 1
       allocate(vec(n))
    end if
    !
    !Put val as last entry:
    vec(n) = val
    !
    if(allocated(tmp))deallocate(tmp)
  end subroutine append_D

  pure subroutine append_C(vec,val)
    complex(8),dimension(:),allocatable,intent(inout) :: vec
    complex(8),intent(in)                             :: val  
    complex(8),dimension(:),allocatable               :: tmp
    integer                                        :: n
    !
    if (allocated(vec)) then
       n = size(vec)
       allocate(tmp(n+1))
       tmp(:n) = vec
       call move_alloc(tmp,vec)
       n = n + 1
    else
       n = 1
       allocate(vec(n))
    end if
    !
    !Put val as last entry:
    vec(n) = val
    !
    if(allocated(tmp))deallocate(tmp)
  end subroutine append_C


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




end module DATA_TUPLE
