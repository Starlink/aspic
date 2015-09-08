      subroutine per_labsim2(n,a,b,x,eps,itmax)
      implicit double precision (a-h,o-z)
      character*72 text
      dimension a(n+1,n+1),b(n+1),x(n+1)
 
*
*       NORMALIZE DIAGONAL ELEMENTS IN EACH ROW
 
      do i=1,n
         x(i)=0.0
         ast=a(i,i)
         b(i)=b(i)/ast
 
         do j=1,n
            a(i,j)=a(i,j)/ast
         end do
 
      end do
 
 
*
*      BEGIN ITERATION
 
      do it=1,itmax
         ifl=1
 
         do i=1,n
            xst=x(i)
            x(i)=b(i)
 
*
*      FIND NEW SOLUTION VALUE
 
            do j=1,n
 
               if (i.ne.j) x(i)=x(i)-a(i,j)*x(j)
            end do
 
 
*
*      TEST FOR CONVERGENCE
 
            if(abs(xst-x(i)).gt.eps) ifl=0
         end do
 
 
         if(ifl.eq.1) return
      end do
 
      write(text,910) itmax
910   format(' No convergence after',i4,' iterations.')
 
      end
 
 
 
