MODULE LIST_TUPLE
  USE SCIFOR
  USE DATA_TUPLE
  implicit none
  private



  type tuple_list
     character(len=:),allocatable :: event
     integer                      :: size=0
     type(tuple),pointer          :: root    => null()
   contains
     procedure,pass      :: free    => free_tuple_list
     procedure,pass      :: append  => append_tuple_list
     procedure,pass      :: get     => get_tuple_list
     procedure,pass      :: read    => read_tuple_list
     !
     procedure,pass      :: rank    => rank_tuple_list
     procedure,pass      :: time    => time_tuple_list
     procedure,pass      :: nat     => nat_tuple_list
     procedure,pass      :: age     => age_tuple_list
     procedure,pass      :: mf      => mf_tuple_list
     procedure,pass      :: speed   => speed_tuple_list
     !
     procedure,pass      :: pdf     => pdf_tuple_list
     procedure,pass      :: interp  => interp_tuple_list
     procedure,pass      :: show    => show_tuple_list
  end type tuple_list


  !GENERIC CONSTRUCTOR
  interface tuple_list
     module procedure :: construct_from_data
     module procedure :: construct_from_tuple
  end interface tuple_list

  !INTRINSIC FUNCTION SIZE(TUPLE_LIST)
  intrinsic :: size
  interface size
     module procedure :: size_tuple_list
  end interface size

  public :: tuple_list
  public :: size

contains



  !##################################################################
  !##################################################################
  !       LIST CONSTRUCTOR/DESTRUCTOR
  !##################################################################
  !##################################################################
  !+------------------------------------------------------------------+
  !PURPOSE:  Intrinsic constructor: given a tuple
  !+------------------------------------------------------------------+
  function construct_from_data(event,rank,time,nat,age,mf,speed) result(self)
    character(len=*) :: event
    integer          :: rank
    character(len=*) :: time
    character(len=3) :: nat
    integer          :: age
    character(len=1) :: mf
    real(8)          :: speed
    type(tuple_list) :: self
    type(tuple)      :: t
    call self%free()
    allocate(self%root)
    self%event=str(event)
    t = tuple(rank,time,nat,age,mf,speed)
    call self%append(t)
  end function construct_from_data

  !+------------------------------------------------------------------+
  !PURPOSE:  Intrinsic constructor: given a list of keys+operators
  !+------------------------------------------------------------------+
  function construct_from_tuple(event,t) result(self)
    character(len=*) :: event
    type(tuple)      :: t
    type(tuple_list) :: self
    call self%free()
    allocate(self%root)
    self%event=str(event)
    call self%append(t)
  end function construct_from_tuple




  !+------------------------------------------------------------------+
  !PURPOSE:  Free an operators_list (destructor) 
  !+------------------------------------------------------------------+
  recursive subroutine free_tuple_list(self)
    class(tuple_list),intent(inout) :: self
    type(tuple),pointer             :: p,c
    if(.not.associated(self%root))return
    do
       p => self%root
       c => p%next
       if(.not.associated(c))exit  !empty list
       p%next => c%next
       call c%free()
       c%next => null()
       deallocate(c)
    enddo
    if(allocated(self%event))deallocate(self%event)
    self%size=0
    self%root=>null()
    p=>null()
    c=>null()
  end subroutine free_tuple_list





  !##################################################################
  !##################################################################
  !       APPEND/READ & OPERATORS 
  !##################################################################
  !##################################################################
  !+------------------------------------------------------------------+
  !PURPOSE:  Append == Put a sparse matrix as operator in the operators_list
  !+------------------------------------------------------------------+
  subroutine append_tuple_list(self,t)
    class(tuple_list),intent(inout) :: self
    type(tuple),intent(in)              :: t
    type(tuple),pointer                 :: p,c
    logical                             :: iupdate
    !
    if(.not.associated(self%root))allocate(self%root)
    !
    p => self%root
    c => p%next
    do                            !traverse the list until QN is found
       if(.not.associated(c))exit
       p => c
       c => c%next
    end do
    !
    allocate(p%next)
    p%next%rank    = t%rank
    p%next%time    = t%time
    p%next%nat     = t%nat
    p%next%age     = t%age
    p%next%mf      = t%mf
    p%next%speed   = t%speed
    p%next%index   = p%index+1
    if(.not.associated(c))then !end of the list special case (c=>c%next)
       p%next%next  => null()
    else
       p%next%next  => c      !the %next of the new node come to current
    end if
    self%size = self%size+1

    p=>null()
    c=>null()
  end subroutine append_tuple_list



  !+------------------------------------------------------------------+
  !PURPOSE: Return tuple from the tuple_list
  !+------------------------------------------------------------------+
  function get_tuple_list(self,index) result(t)
    class(tuple_list)   :: self
    integer,optional    :: index
    type(tuple)         :: t
    integer             :: index_
    type(tuple),pointer :: c
    logical             :: ifound
    !
    index_=self%size;if(present(index))index_=index
    if(index_>self%size)stop "get_tuple_list: index !in [1,self.size]"
    if(index_<0)index_=self%size+index_+1
    !
    ifound=.false.
    c => self%root%next
    loop:do                            !traverse the list until QN is found
       if(.not.associated(c))exit
       if(c%index == index_)then
          ifound=.true.
          exit
       endif
       c => c%next
    end do loop
    if(.not.ifound)stop "get_tuple_list error: not found"
    !
    t = c
    !
    c=>null()
  end function get_tuple_list





  subroutine read_tuple_list(self,event,file)
    class(tuple_list),intent(inout) :: self
    character(len=*),intent(in)     :: event
    character(len=*),intent(in)     :: file
    type(tuple)                     :: t
    integer                         :: i,L,unit
    L = file_length(file)
    open(free_unit(unit),file=trim(file))
    do i=1,L
       call t%read(unit)
       call self%append(t)
    enddo
    close(unit)
    self%event=str(event)
  end subroutine read_tuple_list








  function rank_tuple_list(self) result(data)
    class(tuple_list),intent(inout)      :: self
    real(8),dimension(:),allocatable     :: data
    type(tuple)                          :: t
    integer                              :: i
    !
    if(allocated(data))deallocate(data)
    !
    allocate(data(size(self)))
    do i=1,size(self)
       t = self%get(index=i)
       data(i) = dble(t%rank)
    enddo
    call t%free()
  end function rank_tuple_list



  function time_tuple_list(self,gender,age,period,nat) result(data)
    class(tuple_list),intent(inout)      :: self
    character(len=1),intent(in),optional :: gender
    integer,intent(in),optional          :: age
    integer,intent(in),optional          :: period(2)
    character(len=*),intent(in),optional :: nat
    logical,dimension(4)                 :: flag
    real(8),dimension(:),allocatable     :: data
    type(tuple)                          :: t
    integer                              :: i,a,b
    if(allocated(data))deallocate(data)
    flag = [present(gender),present(age),present(period),present(nat)]
    if( any(flag))then
       do i=1,size(self)
          t = self%get(index=i)
          if(present(gender))then
             if(to_lower(t%mf)==to_lower(gender))call append(data,t%time)
          elseif(present(age))then
             if(t%age==age)call append(data,t%time)
          elseif(present(period))then
             a = period(1);b = period(2)
             if(a>b)then
                a = period(2);b = period(1)
             endif
             if(a<=t%age.AND.t%age<=b)call append(data,t%time)
          elseif(present(nat))then
             if(to_lower(t%nat)==to_lower(nat))call append(data,t%time)
          else
             stop "time_tuple_list error: filter not recognized. "
          endif
       enddo
    else
       allocate(data(size(self)))
       do i=1,size(self)
          t = self%get(index=i)
          data(i) = t%time
       enddo
    endif
    call t%free()
  end function time_tuple_list



  function speed_tuple_list(self,gender,age,period,nat) result(data)
    class(tuple_list),intent(inout)      :: self
    character(len=1),intent(in),optional :: gender
    integer,intent(in),optional          :: age
    integer,intent(in),optional          :: period(2)
    character(len=*),intent(in),optional :: nat
    logical,dimension(4)                 :: flag
    real(8),dimension(:),allocatable     :: data
    type(tuple)                          :: t
    integer                              :: i,a,b
    if(allocated(data))deallocate(data)
    flag = [present(gender),present(age),present(period),present(nat)]
    if( any(flag))then
       do i=1,size(self)
          t = self%get(index=i)
          if(present(gender))then
             if(to_lower(t%mf)==to_lower(gender))call append(data,t%speed)
          elseif(present(age))then
             if(t%age==age)call append(data,t%speed)
          elseif(present(period))then
             a = period(1);b = period(2)
             if(a>b)then
                a = period(2);b = period(1)
             endif
             if(a<=t%age.AND.t%age<=b)call append(data,t%speed)
          elseif(present(nat))then
             if(to_lower(t%nat)==to_lower(nat))call append(data,t%speed)
          else
             stop "speed_tuple_list error: filter not recognized. "
          endif
       enddo
    else
       allocate(data(size(self)))
       do i=1,size(self)
          t = self%get(index=i)
          data(i) = t%speed
       enddo
    endif
    call t%free()
  end function speed_tuple_list


  function age_tuple_list(self,time,speed) result(data)
    class(tuple_list),intent(inout)  :: self
    real(8),intent(in),optional      :: time(2)
    real(8),intent(in),optional      :: speed(2)
    logical,dimension(2)             :: flag
    integer,dimension(:),allocatable :: data
    type(tuple)                      :: t
    integer                          :: i,a,b
    if(allocated(data))deallocate(data)
    flag = [present(time),present(speed)]
    if( any(flag) )then
       do i=1,size(self)
          t = self%get(index=i)
          if(present(time))then
             a = time(1);b = time(2)
             if(a>b)then
                a = time(2);b = time(1)
             endif
             if(a<=t%time.AND.t%time<=b)call append(data,t%age)
          elseif(present(speed))then
             a = speed(1);b = speed(2)
             if(a>b)then
                a = speed(2);b = speed(1)
             endif
             if(a<=t%speed.AND.t%speed<=b)call append(data,t%age)
          else
             stop "age_tuple_list error: filter not recognized. "
          endif
       enddo
    else
       allocate(data(size(self)))
       do i=1,size(self)
          t = self%get(index=i)
          data(i) = t%age
       enddo
    endif
    call t%free()
  end function age_tuple_list



  function nat_tuple_list(self) result(data)
    class(tuple_list),intent(inout)           :: self
    character(len=3),dimension(:),allocatable :: data
    type(tuple)                               :: t
    integer                                   :: i
    if(allocated(data))deallocate(data)
    allocate(data(size(self)))
    do i=1,size(self)
       t = self%get(index=i)
       data(i) = t%nat
    enddo
    call t%free()
  end function nat_tuple_list


  function mf_tuple_list(self) result(data)
    class(tuple_list),intent(inout) :: self
    character(len=1),dimension(:),allocatable :: data
    type(tuple)                     :: t
    integer                         :: i
    if(allocated(data))deallocate(data)
    allocate(data(size(self)))
    do i=1,size(self)
       t = self%get(index=i)
       data(i) = t%mf
    enddo
    call t%free()
  end function mf_tuple_list





  function interp_tuple_list(self,data,L,Npoly) result(odata)
    class(tuple_list),intent(inout)  :: self
    real(8),dimension(:),intent(in)  :: data
    integer,intent(in)               :: L
    integer,intent(in),optional      :: Npoly
    real(8),dimension(:),allocatable :: odata
    type(tuple)                      :: t
    integer                          :: i,N
    N = 10;if(present(Npoly))N=Npoly
    if(allocated(odata))deallocate(odata)
    allocate(odata(L))
    call poly_spline(&
         linspace(0d0,1d0,size(data)),data,&
         linspace(0d0,1d0,L),odata,N)
  end function interp_tuple_list


  function pdf_tuple_list(self,N,type) result(pdf)
    class(tuple_list),intent(inout)      :: self
    integer,intent(in)                   :: N
    character(len=*),intent(in),optional :: type
    character(len=10)                    :: type_
    type(pdf_kernel)                     :: pdf
    real(8),dimension(:),allocatable     :: data
    real(8)                              :: a,b,sigma
    type_='time';if(present(type))type_=type
    select case(str(to_lower(type_)))
    case ("t","time")
       data = self%time()
    case ("s","speed")
       data = self%speed()
    case default
       stop "pdf_tuple_list error: type not in [time,speed]"
    end select
    a = 0d0 ; b = 1.5*data(size(data))
    call pdf_allocate(pdf,N)
    call pdf_set_range(pdf,a,b)
    call pdf_sigma(pdf,data,sigma)
    call pdf_push_sigma(pdf,sigma)
    call pdf_accumulate(pdf,data)
  end function pdf_tuple_list



  !##################################################################
  !##################################################################
  !              ENUMERATOR & ITERATORS
  !##################################################################
  !##################################################################
  !+------------------------------------------------------------------+
  !PURPOSE:  Returns the size of given operators_list
  !+------------------------------------------------------------------+
  function size_tuple_list(self) result(size)
    class(tuple_list),intent(in) :: self
    integer                      :: size
    size = self%size
  end function size_tuple_list








  !##################################################################
  !##################################################################
  !               SHOW 
  !##################################################################
  !##################################################################
  !+------------------------------------------------------------------+
  !PURPOSE:  Pretty print an operators_list
  !+------------------------------------------------------------------+
  recursive subroutine show_tuple_list(self)
    class(tuple_list),intent(inout) :: self
    integer                         :: i,count=0
    type(tuple),pointer             :: c
    !
    write(*,"(A6,I12)")"Size :",self%size
    write(*,"(A6,A)")"Size :",self%event
    write(*,"(A18)")"------------------"
    c => self%root%next
    do
       if(.not.associated(c))exit
       count=count+1
       call c%show()
       c => c%next
    end do
    c=>null()
  end subroutine show_tuple_list




END MODULE LIST_TUPLE
