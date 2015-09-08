      subroutine per_instor (out,n,m,t,v,rubbish)
 
*
*      Subroutine INSTOR
*
*         Copies the data in arrays T and V into a 2-D array OUT,
*         adding a weight of 1.0 or 0.0, depending whether the
*         V value is not/is equal to RUBBISH
*
*         All the arrays are double precision.
*
*   Written by C.D.Pike at RGO
*   Tidied by K.F.Hartley at RGO on 23-1-84
*
      double precision out(n,m),t(10000),v(10000)
 
      do i=1,m
         out(1,i)=t(i)
         out(2,i)=v(i)
 
         if(v(i).eq.rubbish) then
            out(3,i) = 0.0
 
         else
            out(3,i) = 1.0
         endif
 
      end do
 
 
      end
 
 
 
