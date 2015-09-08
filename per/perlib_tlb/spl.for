      function per_spl(x,xc,h)
      implicit double precision (a-h,o-z)
 
*
*    Computes value of the spline function with centre in XC and
*    "STEP" H, i.e. SPL(X,XC,H) is non-zero in  (XC-2H,XC+2H)
      a=abs(x-xc)
 
      if(a.gt.h) goto 1
      p=h-a
      q=p+h
      per_spl=q*q*q-4.*p*p*p
      return
1     continue
 
      if(a.gt.(2.*h)) goto 2
      p=2.*h-a
      per_spl=p*p*p
      return
2     continue
      per_spl=0.
      return
 
      end
 
 
 
 
*
