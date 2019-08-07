! Last change EPP 8/8/2006 4:26PM
 program dupuit_flow_to_fixed_heads
 
 IMPLICIT DOUBLE PRECISION (A-H, O-Z)
 IMPLICIT INTEGER (I-N)
 Real K
 dimension x(1000)
 
 
  OPEN(90,status='old',file="dupuit.in")
  OPEN(91,status='replace',file="dupuit.out")
 
  
  read(90,*) h1, h2, width
  read(90,*) K, w
  read(90,*) n
  
  do i=1,n
    read(90,*) x(i)
  enddo
  
  write(91,*)'Hleft          Hright         Width'
  write(91,3)h1,h2,width
3 format(3f15.5)
  write(91,*)'K              RechargeRate'
  write(91,1)K,w
  write(91,*)      
  write(91,*)'     x         head'
  
  do i=1,n
  hin=(h1*h1)-(((h1*h1-h2*h2)*x(i))/width)+((w/K)*(width-x(i))*x(i))
  h=sqrt(hin)
  write(91,1)x(i),h
  enddo  
  
1 format(2f15.5)
  
  write(91,*)      
  write(91,*)'     q at x=zero'
  
  q= ((K*(h1*h1-h2*h2))/(2.*width))-w*(width/2.)
  write(91,2)q
  
  write(91,*)      
  write(91,*)'     q at x=width'
  
  q= ((K*(h1*h1-h2*h2))/(2.*width))-w*((width/2.)-width)
  write(91,2)q
  
2 format(f15.5)
  
  end program dupuit_flow_to_fixed_heads