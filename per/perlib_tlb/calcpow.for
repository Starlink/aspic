      subroutine per_calcpow(freq,c,s,p,n,m)
 
*
*      Computes and stores power spectrum and corresponding frequencies
*      from (DOUBLE PRECISION) Cos and Sin parts of the FT
*
      double precision c(m),s(m),freq(m)
      double precision p(n,m)
 
      do i = 1,m
         p(1,i)=freq(i)
         p(2,i)=c(i)*c(i)+s(i)*s(i)
      end do
 
 
      end
 
 
 
