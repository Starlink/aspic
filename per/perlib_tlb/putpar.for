      subroutine per_putpar(f,a,p,nf,out,do)
 
*
*   Stores the computed parameters as a 2-D array
*
*   Written by K.F.Hartley at RGO 23-2-84
*
      integer do(2)
      double precision f(nf),a(nf),p(nf),out(do(1),do(2))
 
      do i=1,nf
         out(1,i)=f(i)
         out(2,i)=a(i)
         out(3,i)=p(i)
      end do
 
 
      end
 
 
 
