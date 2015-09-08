      subroutine per_evalu(ia,ib,freq,array,time,d,deltar,cc,ss,cs,yc
     : ,ys)
 
*
*   Subroutine to evaluate the computed values from the current fit?
*
*   Written by C.D.Pike at RGO
*
      implicit double precision (a-h,o-z)
      double precision array(10000),time(10000)
 
*
*     Initialize
*
      cc=0.0
      ss=0.0
      cs=0.0
      yc=0.0
      ys=0.0
      f=6.28318531*freq
 
      do i=ia,ib
         avalu=time(i)*f
         ancos=cos(avalu)
         ansin=sin(avalu)
         cc=cc+ancos**2
         ss=ss+ansin**2
         cs=cs+ancos*ansin
         yc=yc+array(i)*ancos
         ys=ys+array(i)*ansin
      end do
 
      d=(cc*ss-cs**2)
 
      if(abs(d).lt.1.0e-20)call per_abort(9)
      deltar=(yc*(ss*yc-2.0*cs*ys)+cc*ys**2)/d
 
      end
 
 
 
