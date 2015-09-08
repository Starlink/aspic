      subroutine per_sumsq(sumsq,sum,data,time,ipoint,c,phi,fknown,
     :ifreq)
      implicit double precision (a-h,o-z)
      double precision data(ipoint),time(ipoint)
      double precision c(21),phi(20),fknown(20)
      sumsq = 0.0
      sum=0.0
 
      do i=1,ipoint
         asum=data(i)-c(1)
         avalu=6.28318531*time(i)
 
         do j=1,ifreq
            asum=asum-c(j+1)*sin(avalu*fknown(j)+phi(j))
         end do
 
         sum=sum+asum
         sumsq=sumsq+asum**2
      end do
 
 
      end
 
 
 
