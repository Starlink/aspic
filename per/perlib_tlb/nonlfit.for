      subroutine per_nonlfit(data,ax,fknown,ifreq,c,phi,tmean,epoch)
 
*
*   Does a non-linear least squares fit of up to 20 sine waves,
*   refining initial estimates of frequency, amplitude and phase
*   into final estimates.
*
*   Written by C.D.Pike at RGO
*
      implicit double precision (a-h,o-z)
      integer ax(2)
      double precision vector(61),rhs(61)
      double precision matrix(61,61),soln(61)
      double precision  sig(61),phi(20),c(21),fknown(20)
      double precision data(ax(1),ax(2)),time(10000),reduce(10000)
      double precision tmean
      character*72 text
 
*
*   Open a file for the report.
*
      open (unit=3,file='NONRES.LIS',status='UNKNOWN')
 
*
*  Set no of points
*
      ipoint = ax(2)
 
*
*   Set data into correct places and subtract means
*
      epoch = data(1,1)
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
 
*
*        REDUCE(I) = REDUCE(I) - DMEAN
*
         time(i) = time(i) - tmean
      end do
 
 
*
      norder=3*ifreq+1
      nplus1=norder+1
      nfreq1=ifreq+1
      error=1.0e-7*float(ipoint-norder)
      eps=1.0d-5
      call per_sumsq(check,sum,reduce,time,ipoint,c,phi,fknown,ifreq)
      write(3,907)
907   format(/' Non-linear least squares fit adjustment:'/)
      write(3,908)
908   format(' Initial values:...')
      write(3,909)c(1),(c(i+1),i=1,ifreq)
909   format(11x,6(f12.5,3x))
      vector(1)=c(1)
 
      do i=1,ifreq
         vector(i+1)=c(i+1)
         vector(i+nfreq1)=phi(i)
         vector(i+2*ifreq+1) = fknown(i)
      end do
 
 
      do ival=1,25
 
         do i=1,61
 
            do j=1,61
               matrix(i,j)=0.0
            end do
 
         end do
 
         sumsqr = 0.0
 
         do k=1,ipoint
            valdif=reduce(k)-c(1)
            angle=6.28318531*time(k)
 
            do i=1,ifreq
               valdif=valdif-c(i+1)*sin(angle*fknown(i)+phi(i))
            end do
 
            sumsqr = sumsqr + valdif*valdif
 
            do i=1,norder
               pdiffi=per_pdiff(i,angle,ifreq,c,phi,fknown)
 
               do j=1,i
                  matrix(i,j)=matrix(i,j)+pdiffi*per_pdiff(j,angle,
     :            ifreq,c,phi,fknown)
               end do
 
               matrix(i,nplus1)=matrix(i,nplus1)+pdiffi*valdif
            end do
 
         end do
 
         chisq = sumsqr/real(ipoint-norder+1)
         write(3,*) ' CHISQ = ',chisq
 
*     IF(CHISQ.LT.1.0) CHISQ = 1.0
 
         do i=1,norder
 
            do j=1,i
               matrix(j,i)=matrix(i,j)
            end do
 
            rhs(i)=matrix(i,nplus1)
         end do
 
         deter = per_simul(norder,matrix,soln,eps,0,61)
 
         if(abs(deter).lt.1.0e-10.and.ival.eq.1)goto 300
 
         if(abs(deter).lt.1.0e-10)call per_abort(21)
         test=0.0
 
         do i=1,norder
            test=test+rhs(i)*soln(i)
         end do
 
 
         if(test.gt.0.0)goto 100
 
         do i=1,norder
            soln(i)=0.0-soln(i)
         end do
 
100      continue
 
         do i=1,norder
            vector(i)=vector(i)+soln(i)
         end do
 
         c(1)=vector(1)
 
         do i=1,ifreq
            c(i+1)=vector(i+1)
            phi(i)=vector(nfreq1+i)
            fknown(i) = vector(i+2*ifreq+1)
         end do
 
         call per_sumsq(test,sum,reduce,time,ipoint,c,phi,fknown,ifreq)
         write(3,*) 'MEAN LEVEL = ',c(1)
 
         do i=1,ifreq
            write(3,912) fknown(i),c(i+1),phi(i)
912         format(1x,'FREQ,AMP,PHASE OF THIS ITERATION',3f10.6)
         end do
 
         write (text,923) ival
923      format (1h ,i2,' iterations completed')
         call wruser(text,status)
 
         if(abs(test-check).lt.error)go to 200
         check=test
      end do
 
      call wruser('Failed to converge in 25 iterations',status)
      call wruser('However the latest results are retained',status)
200   continue
 
      do i=1,norder
         sig(i) = sqrt(matrix(i,i)*chisq)
         write(3,*) 'ERROR IN PARAMETER',i,' = ',sig(i)
      end do
 
      write(3,913)
913   format(/14x,'FREQUENCY',9x,'PERIOD',16x,'AMPLITUDE',20x,'PHASE')
 
      do i=1,ifreq
         period=1.0/fknown(i)
         write(3,914)fknown(i),period,c(i+1),phi(i)
914      format(11x,f12.7,3x,3(f12.7,13x))
      end do
 
      return
300   continue
      write(3,915)
915   format(' SOLUTION MATRIX IS SINGULAR :USING LINEAR FIT.')
      return
 
      end
 
 
 
