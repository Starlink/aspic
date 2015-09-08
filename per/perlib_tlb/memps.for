      subroutine per_memps(gam,m,freq,step,step1,pm,out,axes)
 
*
*   Computes a maximum entropy estimate of the power spectrum
*   of a dataset described by the coefficients of an AR filter
*   (stored in array GAM). The output together with its
*   corresponding frequencies is stored in OUT
*
*   Written by K.F.Hartley at RGO, based on an earlier version
*   by C.D.Pike, which in turn was based on an algorithm by
*   Ulrych and Bishop.
*
      implicit double precision (a-h,o-z)
      integer m,axes(2)
      double precision gam(m),out(axes(1),axes(2)),freq(2)
      character*72 text
 
*
*   Set up initial values
*
      f = freq(1)
      ifreq = axes(2)
 
*
*   Loop through the frequencies
*
 
      do i=1,ifreq
 
*
*      SR is the REAL part, SI the imaginary one
*
         sr=0.0
         si=0.0
 
         do j=1,m
            sr=sr+gam(j)*dcos(6.283185307d0*f*dble(j))
            si=si+gam(j)*dsin(6.283185307d0*f*dble(j))
         end do
 
         sr = 1.0d0 - sr
 
*
*      Finally re-scale and store
*
         out(1,i) = f/step1
         out(2,i) = pm/(sr*sr+si*si)
         f = f + step
 
         if (mod(i,50).eq.0) then
            write (text,900) ifreq-i
900         format (1h ,i6,' frequencies remaining')
            call wruser(text,status)
         end if
 
      end do
 
 
      end
 
 
 
