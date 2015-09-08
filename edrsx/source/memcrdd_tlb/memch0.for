      subroutine memch0(a,b,size,rinval,nbin,nsum,asum,bsum,
     :                  b2sum,sigma,ierr)
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*SOURCE:
*	MEMCH0.FOR in MEMCRDD.TLB
*-----------------------------------------------------------------------
*
      implicit none
*
      integer	size,nbin,ierr
      real 	a(size),b(size)
      integer 	nsum(nbin)
      real      sigma(nbin),asum(nbin),bsum(nbin),rinval,b2sum(nbin)

*
      real	aamax,aamin,a0,a1,aval,bval,xmax,xmin,ymax,ymin,delta
      integer	sample,i,ibin,minpix,nsamp,ndata,istat,obin
      character device*30

      if(ierr.ne.0) goto 999
 
*
* FIND MAX AND MIN DATA VALUES.
*
      aamax=-1.0E32
      aamin=1.0E32

      do sample=1,size
         aamax=max(aamax,a(sample))
         aamin=min(aamin,a(sample))
      enddo

*
* SET CONSTANTS TO CONVERT INTENSITY INTO BINS
*
      a0=1.5-aamin*(nbin-1)/max(1.0e-20,aamax-aamin)
      a1=(nbin-1)/max(1.0e-20,aamax-aamin)
 
*
* INITIALLISE THE BINS
*
 
      do i=1,nbin
         nsum(i)=0
         asum(i)=0.0
         bsum(i)=0.0
         b2sum(i)=0.0d0
      enddo 
 
*
* SCAN THE DATA
*
 
      do sample=1,size
 
*
* USE PIXELS WHICH ARE OK ON BOTH IMAGES
*
 
         if(a(sample).ne.rinval.and.b(sample).ne.rinval) then
 
            aval=a(sample)
            bval=b(sample)

*
* CALCULATE THE BIN NUMBER AND FORM SUMS FOR THIS BIN
*
            ibin=a0+a1*aval
            nsum(ibin)=nsum(ibin)+1
            bsum(ibin)=bsum(ibin)+bval
            b2sum(ibin)=b2sum(ibin)+bval**2
            asum(ibin)=asum(ibin)+aval
 
         endif
 
      enddo 
 
*
* CALCULATE MIN NO. OF BINS TO USE
*
      minpix=15
      write(*,*) 'MEMCH0: MINPIX = ',minpix
 
*
* JOIN TOGETHER BINS WHICH HAVE INSUFFICIENT POPULATION
*
      obin=1
      ibin=1

      do while(ibin.le.nbin)

         nsum(obin)=nsum(ibin)
         asum(obin)=asum(ibin)
         bsum(obin)=bsum(ibin)
         b2sum(obin)=b2sum(ibin)

         do while(nsum(obin).lt.minpix.and.ibin.le.nbin)
            ibin=ibin+1
       
            nsum(obin)=nsum(obin)+nsum(ibin)
            asum(obin)=asum(obin)+asum(ibin)
            bsum(obin)=bsum(obin)+bsum(ibin)
            b2sum(obin)=b2sum(obin)+b2sum(ibin)

         enddo

         obin=obin+1
         ibin=ibin+1

      enddo

* 
* FORM SIGMA FOR EACH BIN AND FIND EXTENT OF PLOT.
*
      ndata=obin-1
 
      xmax=-1.0e32
      xmin=1.0e32
      ymax=-1.0e32
      ymin=1.0e32

      do i=1,ndata
         sigma(i)=0.0
         asum(i)=asum(i)/nsum(i)
         bsum(i)=sqrt(max(0.0,b2sum(i)/nsum(i)))
         xmax=max(xmax,asum(i))
         xmin=min(xmin,asum(i))
         ymax=max(ymax,bsum(i))
         ymin=min(ymin,bsum(i))
      enddo 
 
*
* PLOT GRAPH
*
      call ncropn('DEVICE',device,.true.,istat)
      if(istat.eq.0) then
         call sgs_sfont(101)
         call ncrbck(xmin-2.0,xmax+2.0,ymin-2.0,ymax+2.0,'Error plot',
     :               'Blurred data value','RMS error')
         call drebar(asum,bsum,sigma,ndata)
      endif

*
* RESAMPLE THE NOISE FUNCTION ONTO EVENLY SPACED POINTS
*
      delta=(aamax-aamin)/nbin

      xmax=-1.0e32
      xmin=1.0e32
      ymax=-1.0e32
      ymin=1.0e32

      do i=1,nbin
         aval=(real(i)-0.5)*delta+aamin

         ibin=1
         do while(ibin.lt.ndata.and.asum(ibin).lt.aval)
            ibin=ibin+1
         enddo

         b2sum(i)=bsum(ibin-1)+(aval-asum(ibin-1))*
     :               (bsum(ibin)-bsum(ibin-1))/(asum(ibin)-asum(ibin-1))

      enddo

*
* FIND THE NOISE VALUES USING THE A VALUES TO INDEX THE EVENLY SPACED
* NOISE FUNCTION
*
      do sample=1,size
         ibin=max(1,min(nbin,nint(0.5+(a(sample)-aamin)/delta)))
         b(sample)=b2sum(ibin)
      enddo


  999 call sgs_close
 
      end
