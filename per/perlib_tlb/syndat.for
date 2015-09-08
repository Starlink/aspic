      subroutine per_syndat(in,out,n,m,freq,amp,phase,cons,noise,ifreq
     : ,tzero)
 
*
*      Takes epochs from IN
*      and builds up sine waves using PARAMS
*      and stores the results in OUT
*
      double precision in(n,m),out(n,m),t,tzero
      real freq(1),amp(20),phase(20),cons,noise
      double precision g05ddf
 
*
*   First initialize the random number generator
*
      nn = secnds(0.0)*100
 
      if(jmod(nn,2).eq.0) nn = nn-1
      call g05cbf(nn)
 
*
*   Now start to fill the arrays
*
 
      do i=1,m
 
*
*      First the epoch and weight
*
         out(1,i)=in(1,i)
 
         if (n.eq.3) out(3,i)=1.0
 
*
*      Note that the times are calculated from the epoch TZERO
*
         t = in(1,i) - tzero
 
*
*      Then add Gaussian noise
*
         ans = cons + g05ddf(0.0,noise)
 
*
*      and build up the contributions from the sine waves
*
 
         do k=1,ifreq
            ans = ans + amp(k)*sin(6.2831853*freq(k)*t+phase(k))
         end do
 
         out(2,i) = ans
      end do
 
 
      end
 
 
 
