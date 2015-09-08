      subroutine per_sumsqu(iflag,sumsq,sum,data,time,ipoint,a,b,c,phi
     : ,fknown,ifreq)
 
*
*   This computes the sums of squares used by the least squares
*   fitting subroutine
*
      implicit double precision (a-h,o-z)
      double precision data(ipoint),time(ipoint)
      double precision a(21),b(20),c(21),phi(20),fknown(20)
      sumsq = 0.0
      sum=0.0
 
      if(iflag.eq.1)   then
 
         do i=1,ipoint
            asum=data(i)-a(1)
            avalu=6.28318531*time(i)
 
            do j=1,ifreq
               avalu=avalu*fknown(j)
               asum=asum-a(j+1)*cos(avalu)-b(j)*sin(avalu)
            end do
 
            sum=sum+asum
            sumsq=sumsq+asum**2
         end do
 
 
      else
 
         do i=1,ipoint
            asum=data(i)-c(1)
            avalu=6.28318531*time(i)
 
            do j=1,ifreq
               asum=asum-c(j+1)*sin(avalu*fknown(j)+phi(j))
            end do
 
            sum=sum+asum
            sumsq=sumsq+asum**2
         end do
 
      end if
 
 
      end
 
 
 
