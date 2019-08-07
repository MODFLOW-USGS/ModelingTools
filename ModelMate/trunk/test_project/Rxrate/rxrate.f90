program rxrate
use subs
implicit none
character(len=200) :: main,pars,exp,out
integer :: kobs, iumain, iupars, iuexp, iout
integer :: i, ts, nx, nTimes
real :: dt, obstime, time, TempB, Temp
real :: CA, ca0, CB, cb0, k1, k2, dCA, dCB
real :: k1b, k2b, e1rat, e2rat

10 format('number of experiments: ',i2)
20 format(i2,',',1x,f5.2,',',1x,f6.2,',',1x,f5.2,',',1x,f5.2,',',1x,f10.7,',',1x,f10.7)

iumain = 7
iupars = 8
iuexp = 9
iout = 10

main = 'main_in'
out = 'rxrate.out.csv'
dt = 0.01

write(*,*)
write(*,*)'Program RXRATE'
open(iout,file=out,status='REPLACE')
open(iumain,file=main,status='OLD') 

read(iumain,*)pars
open(iupars,file=pars,status='OLD')
read(iupars,*)k1b
read(iupars,*)k2b
read(iupars,*)e1rat
read(iupars,*)e2rat
read(iupars,*)TempB
write(*,*)'Parameters read from file: ',trim(pars)
TempB = TempB + 273.15
read(iumain,'(I)')nx
write(iout,10)nx
write(iout,50)
50 format('ID,  TIME,  TEMP,   CA0,   CB0,   CA,         CB')

exploop: do i=1,nx
  read(iumain,*)exp
  open(iuexp,file=exp,status='OLD')
  read(iuexp,*)Temp
  Temp = Temp + 273.15
  read(iuexp,*)CA
  read(iuexp,*)CB
  ca0 = ca
  cb0 = cb
  read(iuexp,*)NTimes
  write(*,30)i,trim(exp),TempB,Temp,CA,CB,NTimes
  30 format(/,'Experiment #:',i2,2x,'File: ',a,/, &
     '  TempB = ',f6.2,/ &
     '  Temp  = ',f6.2,/ &
     '  Ca0   = ',f4.2,/ &
     '  Cb0   = ',f4.2,/ &
     '  # Obs = ',i2)
  time = 0.0
  read(iuexp,*)obstime
  kobs = 1
  k1 = k(k1b,e1rat,TempB,Temp)
  k2 = k(k2b,e2rat,TempB,Temp)
  write(*,40)k1,k2
  40 format('  k1    = ',f4.2,/ &
            '  k2    = ',f4.2)
  tsloop: do ts=1,9999
    if (time==0.0 .or. fuzeq(time,obstime)) then
      write(iout,20)I,time,temp-273.15,ca0,cb0,ca,cb
      if (kobs < ntimes) then
        kobs = kobs + 1
        read(iuexp,*)obstime
      else
        exit tsloop
      endif
    endif
    time = time + dt    
    ! update C's for new time
    dCa = dt*(-k1*CA)
    dCB = dt*(-k2*CB+k1*CA)
    CA = CA + dCA
    CB = CB + dCB
  enddo tsloop
enddo exploop

stop
end program rxrate

