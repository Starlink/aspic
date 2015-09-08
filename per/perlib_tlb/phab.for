      function per_phab(a,b)
 
*
*   Computes a phase from two amplitudes
*
*   Written by K.F.Hartley
*
      implicit double precision (a-h,o-z)
 
      if(a.eq.0.0.and.b.eq.0.0)  then
         call per_abort(8)
         return
      end if
 
 
      if(b.eq.0.0)  then
         per_phab = pi/2.0
 
         if(a.lt.0.0) per_phab = -per_phab
         return
      end if
 
      pi = 3.1415926
 
      if(a/b.lt.0.0)  then
 
         if(a.gt.0.0.and.b.lt.0.0)  per_phab = pi+atan(a/b)
 
         if(a.lt.0.0.and.b.gt.0.0)  per_phab = 2.0*pi + atan(a/b)
 
      else
 
         if(a.gt.0.0)  per_phab = atan(a/b)
 
         if(a.lt.0.0)  per_phab = pi + atan(a/b)
      end if
 
 
      end
 
 
 
