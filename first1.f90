PROGRAM first
IMPLICIT NONE
      integer::i,n
      real*8:: integral,f_1,f_2,a,h,x
      write(*,*) 'Accuracy you want'
      Read(*,*) a
      
      f_1=1.0d0/2.0d0
      f_2=1.0d0/4.0d0
      integral=0.0d0
      n=0
      do while (Abs( 4.0d0*atan(1.0d0)-integral)>a)
      x=0.0d0
      n=n+100
      h=1.0d0/n
      integral=0.0d0
        do i=1,n-1
        x=x+h
        integral=integral+h*(1.0d0/(1+x**2))
        end do
      integral=4*(integral+h*(f_1+f_2))
      write(*,*) n,integral
      end do
      print*, n,integral
END PROGRAM first
      
      

