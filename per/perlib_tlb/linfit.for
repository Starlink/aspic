      subroutine per_linfit(data,ax,fknown,ifreq,a,b,c,phi,tmean,ep)
 
*
*   Does a linear least squares soloution fitting SIN and COS terms
*   with known, fixed frequencies, and finding the amplitudes,
*   converting the result into amplituse and phase of a SIN wave.
*
*   Written by C.D.Pike at RGO
*
      implicit double precision (a-h,o-z)
      double precision a(21),b(20),c(21),fknown(20),phi(20)
      integer ax(2)
      double precision data(ax(1),ax(2)),time(10000),reduce(10000)
 
*
*   First open a file for a log of the results.
*
      open (unit=3,file='LINRES.LIS',status='UNKNOWN')
 
*
*  Set No. of points
*
      ipoint = ax(2)
 
*
*   Set data into correct places and subtract means
*
      dmean = 0.0
      tmean = 0.0
 
      do i=1,ipoint
         reduce(i) = data(2,i)
         time(i) = data(1,i)
         dmean = dmean + reduce(i)
         tmean = tmean + time(i)
      end do
 
      dmean = dmean/real(ipoint)
      tmean = tmean/real(ipoint)
 
      do i=1,ipoint
         reduce(i) = reduce(i) - dmean
         time(i) = time(i) - tmean
      end do
 
 
*
*   Zeroise out accumulators
*
 
      do i=1,20
         a(i)=0.0
         b(i)=0.0
         c(i)=0.0
         phi(i)=0.0
      end do
 
      a(21)=0.0
      c(21)=0.0
      a(1) = dmean
 
*
*    Set convergence limit
*
      error=float(ipoint-2*ifreq-1)*1.0e-3
      call per_sumsqu(1,check,per_sum,reduce,time,ipoint,a,b,c,phi,
     :fknown,ifreq)
 
      do j=1,40
 
*
*     Loop on iteration.  Set max no of iterations to 40
*
         avalu=0.0
 
*
*      Loop through data points
*
 
         do i=1,ipoint
            avalu=avalu+reduce(i)
         end do
 
         avalu=avalu/float(ipoint)
         a(1)=a(1)+avalu
 
*
*      Subtract off mean value
*
 
         do i=1,ipoint
            reduce(i)=reduce(i)-avalu
         end do
 
 
*
*      Loop through known frequencies
*
 
         do k=1,ifreq
            call per_evalu(1,ipoint,fknown(k),reduce,time,d,deltar,cc
     :       ,ss,cs,yc,ys)
            avalu=(yc*ss-ys*cs)/d
            bvalu=(cc*ys-cs*yc)/d
            a(k+1)=a(k+1)+avalu
            b(k)=b(k)+bvalu
            f=6.28318531*fknown(k)
 
            do i=1,ipoint
               angle=f*time(i)
               reduce(i)=reduce(i)-avalu*cos(angle)-bvalu*sin(angle)
            end do
 
         end do
 
         write(3,900)a(1),(a(i+1),i=1,ifreq)
900      format(11x,6(f12.7,3x))
         write(3,910)(b(i),i=1,ifreq)
910      format(26x,5(f12.5,3x))
         call per_sumsqu(1,test,sum,reduce,time,ipoint,a,b,c,phi,fknown
     :    ,ifreq)
         fratio=abs((test-check)/check)
         write(3,920)fratio
920      format(11x,'COMPARATIVE REDUCTION:   ',f12.5)
 
         if(abs(test-check).lt.error)goto 188
         check=test
      end do
 
188   continue
      write(3,930)
930   format (//14x,'Frequency',6x,'Amplitude',10x,'Phase'/)
      c(1)=a(1)
 
      do i=1,ifreq
         c(i+1)=sqrt(a(i+1)**2+b(i)**2)
         phi(i)=per_phab(a(i+1),b(i))
         write(3,950)fknown(i),c(i+1),phi(i)
950      format(11x,3(f12.5,3x))
      end do
 
      write (3,970) a(1),tmean
970   format(//,11x,'Mean value is ',f12.5,//11x,'Working epoch is '
     :,f16.5)
 
*
*   Finally, set EP to the epoch of the first point, for later use.
*
      ep=data(1,1)
 
      end
 
 
 
