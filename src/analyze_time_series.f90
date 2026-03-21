program analysis
  USE SCIFOR
  USE DATA_TUPLE
  USE LIST_TUPLE
  implicit none
  integer,parameter                         :: N=512
  integer                                   :: i
  character(len=12 )                        :: time
  integer                                   :: irace
  integer                                   :: Llist,iunit,ounit,lunit
  character(len=100)                        :: ifile,ofile
  character(len=256)                        :: event
  type(pdf_kernel)                          :: pdf
  real(8)                                   :: a,b,sigma,t0,ta,te
  real(8),dimension(:),allocatable          :: xdata,ydata,sdata
  real(8),dimension(N)                      :: Et,Es
  real(8),dimension(N)                      :: Tt,Ts
  type(tuple)                               :: td
  type(tuple_list),dimension(:),allocatable :: tlist


  open(free_unit(lunit),file='list.race')
  Llist=file_length('list.race')
  allocate(tlist(Llist))
  do irace=1,Llist
     read(lunit,*)event
     ifile="../data/input_"//str(event)//".csv"

     !>read data from file:
     print*,"reading: "//str(event)
     call tlist(irace)%read(event,str(ifile))
  enddo

  !<T(i)>   = 1/Llist*sum_l T_l(i)
  !<<T(i)>> = exp(<ln T(i)>)
  Et = 0d0;Es = 0d0
  Tt = 0d0;Ts = 0d0
  ta = 0d0;te = 0d0
  do irace=1,Llist
     if(size(tlist(irace))==0)cycle
     event = str(tlist(irace)%event)
     write(*,*)"Processing: "//str(event)
     ydata = tlist(irace)%time()
     sdata = tlist(irace)%speed()
     t0    = ydata(1)
     open(free_unit(ounit),file="output_"//str(event)//".dat")
     do i=1,size(tlist(irace))
        write(ounit,*)i/dble(size(tlist(irace))),ydata(i)-t0,sdata(i),ydata(i)
     enddo
     close(ounit)


     !> get interpolated data:
     ydata = tlist(irace)%interp(tlist(irace)%time(),N,3)
     sdata = tlist(irace)%interp(tlist(irace)%speed(),N,3)

     Et = Et + ydata/Llist
     Es = Es + sdata/Llist
     te = te + t0/Llist

     Tt = Tt + log(ydata/t0)/Llist
     Ts = Ts + log(sdata)/Llist
     ta = ta + log(t0)/Llist


     pdf = tlist(irace)%pdf(500,'time')
     call pdf_print(pdf,"PDF_"//str(event)//".dat")
  enddo

  Tt = exp(Tt)
  Ts = exp(Ts)


  open(free_unit(ounit),file="Edata.dat")
  do i=1,N
     write(ounit,*)i/dble(N),Et(i)-te,Es(i),Et(i)
  enddo
  close(ounit)

  open(free_unit(ounit),file="Tdata.dat")
  do i=1,N
     write(ounit,*)i/dble(N),(Tt(i)-1d0)*exp(ta),Ts(i),Tt(i)
  enddo
  close(ounit)



end program analysis
