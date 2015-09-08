      function per_pdiff(i,angle,ifreq,c,phi,fknown)
      implicit double precision (a-h,o-z)
      double precision c(21),phi(20),fknown(20)
 
      if(i.eq.1)  then
         per_pdiff = 1.0
         return
      endif
 
 
      if(i.ge.2.and.i.le.(ifreq+1))  then
         per_pdiff = sin(angle*fknown(i-1)+phi(i-1))
      endif
 
 
      if(i.ge.(ifreq+2).and.i.le.(2*ifreq+1))  then
         ib = i-ifreq-1
         per_pdiff = c(ib+1)*cos(angle*fknown(ib)+phi(ib))
      endif
 
 
      if(i.ge.(2*ifreq+2).and.i.le.(3*ifreq+1))  then
         ib = i-2*(ifreq+1)+1
         per_pdiff = c(ib+1)*cos(angle*fknown(ib)+phi(ib))*angle
      endif
 
 
      end
 
 
 
