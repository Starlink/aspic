      subroutine per_fpowr(x,n,out,m,t)
 
*
*   This subroutine extracts the Fourier transform from the 1-D
*   array X, as stored by NAG routine C06FAF, and stores the
*   corresponding Power Spectrum in OUT
*
      double precision x(n),out(2,m)
      double precision s,c,t,fac
      fac=1.0/sqrt(dble(n))
 
      do i = 1,n/2
         c=x(i)
         s=x(n+1-i)
         out(2,i)=(s*s+c*c)*fac
         k=i
         out(1,i)=dble(k-1)/t
      end do
 
 
      end
 
 
 
