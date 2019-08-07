module subs
contains

real function f1(k1,u1)
implicit none
real :: k1,u1
f1 = -k1*u1
return
end function f1

real function f2(k1,k2,u1,u2)
implicit none
real :: k1,k2,u1,u2
f2 = k1*u1-k2*u2
return
end function f2

logical function fuzeq(a,b)
implicit none
real :: a, b, avg, dif, ckval
avg = abs((a+b)/2.0)
ckval = 1.0e-5*avg
dif = abs(b-a)
fuzeq = dif < ckval
return
end function fuzeq

real function k(kb,erat,TempB,Temp)
implicit none
real :: kb,erat,TempB,Temp
k = kb*exp(erat*(1.0-(TempB/Temp)))
end function k

end module subs
