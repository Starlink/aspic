      subroutine per_mempr(m,f,lg,g,phi,lext,fpe,pm)
 
*
*
*   Subroutine to compute the Burg estimates of the prediction error
*   filter coefficients and the Akaike FPE (Final Prediction Error)
*
*   Published in Reviews of Geophysics and Space Physics 13,183 1975.
*   by Ulrych & Bishop.
*
* F(M) are the input data
* G(LG)  are the prediction error coefficients.
* PHI(LEXT) are the autocorrelation coefficients up to lag LEXT
*
*   The lags LG+1 to LEXT are extrapolated using MEM
*
*   FPE are the log of the normalised FPE's
*   PM is the updated power
*
*   Tidied up by K.F.Hartley 3-2-84
*
      double precision g(lg),per(20000),pef(20000),h(20000),phi(lext)
      double precision sum,sn,sd
      double precision dm
      real fpe(lext)
      double precision f(m)
      sum = 0.0d0
 
      do  i=1,m
         sum = sum + f(i)*f(i)
      end do
 
      phi(1) = sum/real(m)
      pm = phi(1)
      dm = phi(1)
      fpe(1) = real(m+1)/real(m-1)*pm
      ftemp = fpe(1)
      fpe(1) = 0.0
 
      do nn=2,lg
         n= nn-2
 
         if (n.eq.0) then
 
            do j=1,m
               pef(j) = 0.0d0
               per(j) = 0.0d0
            end do
 
         end if
 
         sn = 0.0d0
         sd = 0.0d0
         jj = m-n-1
 
         do j=1,jj
            sn = sn - 2.0d0*(f(j+n+1)+pef(j))*(f(j)+per(j))
            sd = sd + (f(j+n+1)+pef(j))**2 + (f(j)+per(j))**2
         end do
 
         g(nn) = sn/sd
 
         if (n.ne.0) then
 
            do j=1,n
               k = n-j+2
               h(j+1) = g(j+1) + g(nn)*g(k)
            end do
 
 
            do j=1,n
               g(j+1) = h(j+1)
            end do
 
            jj = jj - 1
         end if
 
 
         do j=1,jj
            per(j) = per(j) + g(nn)*pef(j) + g(nn)*f(j+nn-1)
            pef(j) = pef(j+1) + g(nn)*per(j+1) + g(nn)*f(j+1)
         end do
 
         sum = 0.0d0
 
         do j=2,nn
            sum = sum - phi(nn+1-j)*g(j)
         end do
 
         phi(nn) = sum
         dm = (1.0d0 - g(nn)**2)*dm
         pm = dm
 
         if (nn.ne.m) then
            fpe(nn) = real(m+nn)/real(m-nn)*pm
            fpe(nn) = fpe(nn)/ftemp
            fpe(nn) = alog10(fpe(nn))
         end if
 
      end do
 
      g(1) = 1.0d0
      lg1 = lg + 1
 
      do j=lg1,lext
         sum = 0.0d0
 
         do i=2,lg
            sum = sum - phi(j+1-i)*g(i)
         end do
 
         phi(j) = sum
      end do
 
 
      end
 
 
 
