      subroutine nmplot(ia,npixa,nlinea,invala,ascale,azero,ib,npixb
     : ,nlineb,invalb,bscale,bzero,ilevel,amin,amax,nbin,niter,siglim
     :  ,minpix,device,m,c,ierr,nsum,asum,bsum,b2sum,varlim,ttl,ttlx,
     :   ttly)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO NORMALIZE ONE IMAGE TO ANOTHER SIMILAR IMAGE BY PLOTTING
*       THE INTENSITIES IN EACH IMAGE AGAINST EACH OTHER
*
*METHOD
*       INTENSITIES WHICH ARE VALID IN EACH INPUT IMAGE AND LIE WITHIN
*       THE DATA RANGE TO BE USED IN IMAGE A ARE BINNED ACCORDING TO
*       THE INTENSITY IN IMAGE A. A MEAN AND STANDARD DEVIATION FOR THE
*       B INTENSITIES ARE FOUND FOR EACH BIN. A STRAIGHT LINE IS FITTED
*       TO THIS BINNED DATA TO DETERMINE THE SLOPE AND INTERCEPT, FROM
*       WHICH THE B IMAGE MAY BE NORMALIZED TO THE A IMAGE. ITERATIONS
*       ARE PERFORMED TO REJECT BAD DATA BY REPEATING THE BINNING AND
*       LINE FITTING PROCEDURE, REJECTING PIXELS WHOSE B INTENSITIES
*       DEVIATE BY MORE THAN A SPECIFIED NUMBER OF STANDARD DEVIATIONS
*       FROM THE LINE FITTED IN THE PREVIOUS ITERATION.
*
*ARGUMENTS
*       IA (IN)
*       INTEGER*2(NPIXA,NLINEA)
*              FIRST INPUT IMAGE TO BE NORMALIZED TO
*       NPIXA,NLINEA (IN)
*       INTEGER
*              DIMENSIONS OF IMAGE IA
*       INVALA (IN)
*       INTEGER
*              INVALID PIXEL FLAG FOR IA
*       ASCALE,AZERO (IN)
*       REAL
*              SCALE AND ZERO FOR IMAGE IA
*       IB (IN)
*       INTEGER*2(NPIXB,NLINEB)
*              SECOND INPUT IMAGE...THE ONE TO BE NORMALIZED
*       NPIXB,NLINEB (IN)
*       INTEGER
*              DIMENSIONS OF IB
*       INVALB (IN)
*       INTEGER
*              INVALID PIXEL FLAG FOR IB
*       BSCALE,BZERO (IN)
*       REAL
*              SCALE AND ZERO FOR IMAGE IB
*       ILEVEL (IN)
*       INTEGER
*              INTERACTION LEVEL...CONTROLS PRINTING OF RESULTS
*       AMIN,AMAX (IN)
*       REAL
*              THE RANGE OF DATA VALUES TO USE IN IMAGE A
*       NBIN (IN)
*       INTEGER
*              THE NUMBER OF BINS TO USE FOR THE DATA
*       NITER (IN)
*       INTEGER
*              NUMBER OF DATA REJECTION ITERATIONS TO USE
*       SIGLIM (IN)
*       REAL
*              REJECTION THRESHOLD IN STANDARD DEVIATIONS FOR ABERRANT
*              DATA VALUES
*       MINPIX (IN)
*       INTEGER
*              MIN. NUMBER OF DATA VALUES REQUIRED BEFORE A BIN IS USED
*       DEVICE (IN)
*       CHARACTER
*              GKS GRAPHICS DEVICE FOR PLOT OF FINAL FIT
*       M (OUT)
*       REAL
*              SLOPE OF LINE FITTED IN THE EXPRESSION B=M*A+C
*       C (OUT)
*       REAL
*              CONSTANT C IN THE ABOVE EXPRESSION
*       IERR (OUT)
*       INTEGER
*              ERROR FLAG: ZERO FOR SUCCESS
*       NSUM (WORK)
*       INTEGER(NBIN)
*              WORKSPACE
*       ASUM,BSUM (WORK)
*       REAL(NBIN)
*              WORKSPACE
*       B2SUM (WORK)
*       DOUBLE PRECISION(NBIN)
*              WORKSPACE
*       VARLIM (WORK)
*       REAL(NBIN)
*              WORKSPACE
*	TTL (IN)
*	CHARACTER
*	       TITLE FOR PLOT
*	TTLX (IN)
*	CHARACTER
*	       TITLE FOR X AXIS
*	TTLY (IN)
*	CHARACTER
*	       TITLE FOR Y AXIS
*
*CALLS
*       THIS PACKAGE:
*              LBGONE,NCROPN,NCRBCK,DREBAR,WRUSER
*	SGS:
*	       SGS_CLOSE,SGS_SFONT
*	GKS:
*	       GPL
*
*NOTES
*       USES INTEGER*2 ARRAYS AND SUBROUTINE NAMES WITH MORE THAN
*       SIX CHARACTERS
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
      integer*2 ia(npixa,nlinea),ib(npixb,nlineb)
      integer nsum(nbin)
      character prbuf*80,device*(*),ttl*(*),ttlx*(*),ttly*(*)
      real m,varlim(nbin),asum(nbin),bsum(nbin)
      double precision xsum,x2sum,ysum,xysum,wtsum,b2sum(nbin),x,y,wt
     : ,atop,abot
      ierr=0
 
*
* SET MINIMUM EXPECTED VARIANCE FOR A BIN
*
      q0=0.5*bscale**2
      m=1.0
      c=0.0
 
*
* SET CONSTANTS TO CONVERT INTENSITY INTO BINS
*
      aamin=min(amin,amax)
      aamax=max(amin,amax)
      a0=1.5-aamin*(nbin-1)/max(1.0e-20,aamax-aamin)
      a1=(nbin-1)/max(1.0e-20,aamax-aamin)
 
*
* INITIALLISE THE VARIANCE THRESHOLD FOR EACH BIN
*
 
      do 1 i=1,nbin
         varlim(i)=1.0e20
1     continue
 
 
*
* PERFORM NITER ITERATIONS
*
      iter=0
 
37    if(iter.le.niter)then
 
*
* INITIALLISE THE BINS
*
 
         do 2 i=1,nbin
            nsum(i)=0
            asum(i)=0.0
            bsum(i)=0.0
            b2sum(i)=0.0d0
2        continue
 
 
*
* SCAN THE AREA COMMON TO BOTH IMAGES
*
 
         do 99 j=1,min(nlinea,nlineb)
 
            do 98 i=1,min(npixa,npixb)
 
*
* USE PIXELS WHICH ARE OK ON BOTH IMAGES
*
 
               if(ia(i,j).ne.invala.and.ib(i,j).ne.invalb) then
 
*
* CHECK IF THE A INTENSITY IS IN THE REQUIRED DATA RANGE
*
                  a=ia(i,j)*ascale+azero
 
                  if(a.ge.aamin.and.a.le.aamax) then
 
*
* CALCULATE THE BIN NUMBER AND FORM SUMS FOR THIS BIN
*
                     ibin=a0+a1*a
                     b=ib(i,j)*bscale+bzero
                     bfit=a*m+c
 
                     if((b-bfit)**2.le.varlim(ibin))then
                        nsum(ibin)=nsum(ibin)+1
                        bsum(ibin)=bsum(ibin)+b
                        b2sum(ibin)=b2sum(ibin)+b*b
                        asum(ibin)=asum(ibin)+a
                     endif
 
                  endif
 
               endif
 
98          continue
 
99       continue
 
 
*
* COUNT THE TOTAL NUMBER OF PIXELS USED
*
         npix=0
 
         do 3 i=1,nbin
            npix=npix+nsum(i)
3        continue
 
 
*
* CALCULATE THE MAX. WEIGHT TO BE APPLIED TO ANY BIN (THIS IS THE NUMBER
* OF POINTS PER BIN IF THE POINTS ARE UNIFORMLY DISTRIBUTED)
*
         wtmax=real(npix)/real(nbin)
 
*
* INITIALLISE SUMS FOR THE STRAIGHT LINE FIT
*
         xsum=0.0d0
         x2sum=0.0d0
         ysum=0.0d0
         xysum=0.0d0
         wtsum=0.0d0
         abot=1.0d20
         atop=-1.0d20
 
*
* SCAN THOSE BINS WITH AT LEAST THE MINIMUM NUMBER OF PIXELS IN
*
 
         do 4 i=1,nbin
 
            if(nsum(i).ge.minpix) then
 
*
* FORM THE WEIGHT FOR THIS BIN
*
               wt=min(wtmax,real(nsum(i)))
 
*
* SET THE VARIANCE THRESHOLD IN THIS BIN FOR THE NEXT ITERATION
*
               varlim(i)=(siglim**2)*(b2sum(i)-(bsum(i)**2)/nsum(i))
     :          /nsum(i)
               varlim(i)=max(varlim(i),q0*(siglim**2))
 
*
* FORM THE MEAN DATA VALUES IN EACH BIN
*
               x=asum(i)/nsum(i)
               y=bsum(i)/nsum(i)
 
*
* FORM WEIGHTED SUMS FOR FIT
*
               xsum=xsum+x*wt
               x2sum=x2sum+x*x*wt
               ysum=ysum+y*wt
               xysum=xysum+y*x*wt
               wtsum=wtsum+wt
 
*
* NOTE THE RANGE OF VALUES USED
*
               atop=max(atop,x)
               abot=min(abot,x)
 
            else
 
*
* SET MINIMUM VARIANCE FOR BINS WITH TOO FEW POINTS
*
               varlim(i)=(siglim**2)*q0
            endif
 
4        continue
 
 
*
* IF NORMAL EQUATIONS ARE SINGULAR, ABORT WITH IERR=1
*
         det=wtsum*x2sum-xsum*xsum
 
         if(det.eq.0.0) then
            ierr=1
            go to 77
 
         endif
 
 
*
* FORM THE STRAIGHT LINE PARAMETERS (B=M*A+C)
*
         m=(wtsum*xysum-xsum*ysum)
         c=(x2sum*ysum-xsum*xysum)
         m=m/det
         c=c/det
 
*
* IF REQUIRED, PRINT THE RESULTS OF THIS ITERATION
*
 
         if(ilevel.ge.2) then
            write(prbuf,14)iter,npix,abot,atop
14          format('   ITERATION ',i3,' USED ',i10,' PIXELS FROM A='
     :      ,g13.6,' TO ',g13.6)
            call lbgone(prbuf(74:))
            call lbgone(prbuf(58:))
            call lbgone(prbuf(23:))
            call lbgone(prbuf(14:))
            call wruser(' ',istat)
            call wruser(prbuf,istat)
            call wruser(' ',istat)
            write(prbuf,15)m,c
15          format('   FIT GIVES:   B=(',g13.6,')*A + (',g13.6,')')
            call lbgone(prbuf(48:))
            call lbgone(prbuf(28:))
            call wruser(prbuf,istat)
            call wruser(' ',istat)
         endif
 
 
*
* ON THE FINAL ITERATION, PLOT THE FIT IF REQUIRED
*
 
         if(iter.eq.niter.and.device.ne.'NONE') then
 
*
* REMOVE BINS WITH NO DATA IN AND FORM THE VALUES TO PLOT
* ALSO CALCULARTE THE MAX AND MINIMUM EXTENT OF THE PLOT IN X AND Y
* TAKING INTO ACCOUNT THE LENGTH OF THE ERROR BARS
*
            ndata=0
 
            xmax=-1.0e32
            xmin=1.0e32
            ymax=-1.0e32
            ymin=1.0e32
            do 16 i=1,nbin
 
               if(nsum(i).ge.minpix) then
                  ndata=ndata+1
                  asum(ndata)=asum(i)/nsum(i)
                  bsum(ndata)=bsum(i)/nsum(i)
                  varlim(ndata)=sqrt(max(0.0d0,(b2sum(i)-(bsum(i)**2)
     :             *nsum(i))/nsum(i)))
                  xmax=max(xmax,asum(ndata))
                  xmin=min(xmin,asum(ndata))
                  ymax=max(ymax,bsum(ndata)+varlim(ndata))
                  ymin=min(ymin,bsum(ndata)-varlim(ndata))
               endif
 
16          continue
 
 
*
* PLOT GRAPH
*
            call ncropn(device,.true.,ierr)
            if(ierr.ne.0) goto 99
            call sgs_sfont(101)
            call ncrbck(xmin,xmax,ymin,ymax,ttl,ttlx,ttly)
            call drebar(asum,bsum,varlim,ndata)
            asum(2)=asum(ndata)
            bsum(1)=m*asum(1)+c
            bsum(2)=m*asum(2)+c
            call gpl(2,asum,bsum)
            call sgs_close
         endif
 
 
*
* COUNT ITERATIONS AND RETURN FOR NEXT
*
         iter=iter+1
         go to 37
 
      endif
 
77    return
 
      end
 
 
 
