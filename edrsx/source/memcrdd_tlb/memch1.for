      subroutine memch1(a,size,rinval,nbin,nsum,asum,bsum,sigma,ttl,
     :                  ttlx,ttly,ierr)
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*SOURCE:
*	MEMCH0.FOR in MEMCRDD.TLB
*-----------------------------------------------------------------------
*
      implicit none
*
      integer	size,nbin,ierr
      real 	a(size)
      integer 	nsum(nbin)
      real      sigma(nbin),asum(nbin),bsum(nbin),rinval

*
      real	aamax,aamin,a0,a1,aval,xmax,xmin,ymax,ymin
      integer	sample,i,ibin,nsamp,ndata,istat
      character device*30,ttl*(*),ttlx*(*),ttly*(*)

      if(ierr.ne.0) goto 999
 
*
* FIND MAX AND MIN DATA VALUES.
*
      aamax=-1.0E32
      aamin=1.0E32

      do sample=1,size
         aval=a(sample)
         if(aval.ne.rinval) then      
            aamax=max(aamax,aval)
            aamin=min(aamin,aval)
         endif
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
      enddo 
 
*
* SCAN THE DATA
*
 
      do sample=1,size
 
*
* USE PIXELS WHICH ARE OK ON BOTH IMAGES
*
 
         if(a(sample).ne.rinval) then
 
            aval=a(sample)

*
* CALCULATE THE BIN NUMBER AND FORM SUMS FOR THIS BIN
*
            ibin=a0+a1*aval
            nsum(ibin)=nsum(ibin)+1
            asum(ibin)=asum(ibin)+aval
 
         endif
 
      enddo 
 
* 
* FORM EXTENT OF PLOT.
*
      xmax=-1.0e32
      xmin=1.0e32
      ymax=-1.0e32
      ymin=1.0e32

      ndata=0

      open(17,file='MEMVAR.DAT',status='NEW')

      do i=1,nbin
         if(nsum(i).gt.0) then
            ndata=ndata+1
            sigma(ndata)=0.0
            asum(ndata)=asum(i)/nsum(i)
            bsum(ndata)=nsum(i)
            xmax=max(xmax,asum(i))
            xmin=min(xmin,asum(i))
            ymax=max(ymax,bsum(i))
            ymin=min(ymin,bsum(i))
            write(17,*) asum(ndata),bsum(ndata),i
         endif
      enddo 
      close(17)
 
*
* PLOT THE DATA
*
      call ncropn('DEVICE',device,.true.,istat)
      if(istat.eq.0) then
         call sgs_sfont(101)
         call ncrbck(xmin-2.0,xmax+2.0,ymin-2.0,ymax+2.0,ttl,ttlx,ttly)
         call drebar(asum,bsum,sigma,ndata)
      endif

  999 call cnpar('DEVICE',istat)
      call sgs_close
 
      end
