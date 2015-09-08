      subroutine per_labsim(lag,n,rho,y,x,eps,itmax)
 
*
*      PURPOSE: Modified Gauss-Seidel solver for simultaneous linear
*               equations.
*
*      INPUT:   LAG - LAG
*               N - NO OF UNKNOWNS
*               RHO - MATRIX OF AUTOCORRELATION OF LAG COEFS
*               X - RHS OF EQUATION
*
*      OUTPUT:  X GETS OVERWRITTEN WITH SOLUTION VECTOR
*
      implicit double precision (a-h,o-z)
      dimension a(500),rho(500),y(10000),x(10000)
      character*72 text
 
*
*      Normalize diagonal
*
      a1=rho(1)
 
      do i=1,n
         x(i)=0.0
         y(i)=-y(i)/a1
      end do
 
 
      do i=1,lag
         a(i)=rho(i)/a1
      end do
 
 
*
*      Begin iteration
*
 
      do it=1,itmax
         ifl=1
 
*
 
         do i=1,n
            xst=x(i)
            x(i)=y(i)
            jmin=i-lag+1
 
            if(jmin.lt.1) jmin=1
            jmax=i+lag-1
 
            if(jmax.gt.n) jmax=n
 
*
*         Find new solution value
*
 
            do j=jmin,jmax
 
               if (i.ne.j) then
                  ij=iabs(i-j)+1
                  x(i)=x(i)-a(ij)*x(j)
               end if
 
            end do
 
 
*
*      Test X(I) for convergence
*
 
            if(abs(xst-x(i)).gt.eps) ifl=0
         end do
 
 
         if(ifl.ne.0) then
            write (text,900) it
900         format(1h ,'Converged after ',i4,' iterations')
            call wruser(text,istat)
            return
 
         else
 
            if (mod(it,20).eq.0) then
               write (text,910) it
910            format (1h ,i4,' iterations completed.')
               call wruser(text,istat)
            end if
 
         end if
 
      end do
 
      write(text,920) itmax
920   format(' No convergence after',i4,' iterations.')
      call wruser(text,istat)
 
      end
 
 
 
