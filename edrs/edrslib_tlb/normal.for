      subroutine normal
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO NORMALIZE ONE IMAGE TO A SIMILAR IMAGE, DETERMINING THE
*       SCALE AND ZERO DIFFERENCE BETWEEN THE TWO IMAGES BY PLOTTING
*       THE INTENSITIES IN THE TWO IMAGES AGAINST EACH OTHER
*
*METHOD
*       OBTAIN INPUT IMAGES AND EXTRACT DESCRIPTOR ITEMS. CALL PCHIST
*       TO IMPLEMENT AN AUTO-SCALING FUNCTION TO DETERMINE A RANGE OF
*       DATA VALUES TO PLOT. OBTAIN NECESSARY PARAMETERS FROM THE
*       ENVIRONMENT. CALL NMPLOT TO NORMALISE THE TWO IMAGES.
*
*ARGUMENTS
*       NONE
*
*STARLINK PARAMETERS
*       AIMAGE
*              INPUT IMAGE TO NORMALIZE TO
*       BIMAGE
*              INPUT IMAGE TO BE NORMALIZED
*       ILEVEL
*              INTERACTION LEVEL...CONTROLS PRINTING OF RESULTS
*       PCRANGE
*              LOWER AND UPPER HISTOGRAM POINTS (IN PERCENT) DEFINING
*              THE DEFAULT (AUTO-SCALED) DATA RANGE USED IN IMAGE A
*       ARANGE
*              LOWER AND UPPER RANGE OF DATA VALUES IN IMAGE A, USED
*              TO OVERRIDE THE AUTOSCALING
*       NBIN
*              NUMBER OF BINS TO USE IN BINNING THE SCATTER PLOT PRIOR
*              TO FITTING A STRAIGHT LINE
*       NITER
*              NUMBER OF ITERATIONS TO REJECT BAD DATA VALUES
*       NSIGMA
*              NUMBER OF STANDARD DEVIATIONS AT WHICH BAD DATA IS
*              REJECTED
*       MINPIX
*              MINIMUM NUMBER OF GOOD PIXELS REQUIRED IN A BIN BEFORE
*              IT CONTRIBUTES TO THE FITTED LINE
*       DEVICE
*              GRAPHICS DEVICE ON WHICH TO PLOT THE FITTED LINE
*       BNORM
*              DETERMINES IF THE B IMAGE IS TO BE NORMALIZED
*              AUTOMATICALLY
*       E
*              OUTPUT PARAMETER GIVING THE SLOPE IN THE EXPRESSION
*              B=E*A+C
*       C      OUTPUT PARAMETER GIVING THE CONSTANT C IN THE
*              ABOVE EXPRESSION
*	HEADING
*	       TITLE FOR GRAPH
*	XHEAD
*	       TITLE FOR X AXIS
*	YHEAD
*              TITLE FOR Y AXIS
*
*CALLS
*       THIS PACKAGE:
*              GT2DIR,GTDSCR,GETPAR,RNGERR,PCHIST,NMPLOT,PTDSCR,GETDEV,
*	       DEFDEV,GTINAM,LENSTR,UPPERC
*       STARLINK:
*              RDKEYC,RDKEYR,WRERR,CNPAR,RDKEYL,WRKEYR,GETDYN,FRDATA
*
*NOTES
*       USES VAX %VAL FACILITY
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      parameter (minint=-32768,maxint=32767)
      character cval*1,device*30,options*200,ttl*50,ttlx*50,ttly*50
      real e(1),c(1),nsigma,pcrange(2),arange(2)
      integer irange(2),ihist(minint:maxint)
      logical bnorm(1)
 
*
* OBTAIN FIRST INPUT IMAGE
*
      call gt2dir('AIMAGE',102,.false.,npixa,nlinea,ipa,ierra)
 
      if (ierra.eq.0) then
 
*
* OBTAINED SUCCESSFULLY...EXTRACT DESCRIPTOR ITEMS
*
         ascale=1.0
         azero=0.0
         invala=-100000
         call gtdscr('AIMAGE','BSCALE','REAL',ival,ascale,cval,ierr)
         call gtdscr('AIMAGE','BZERO','REAL',ival,azero,cval,ierr)
         call gtdscr('AIMAGE','INVAL','INTEGER',invala,rval,cval,ierr)
 
*
* OBTAIN SECOND INPUT IMAGE
*
         call gt2dir('BIMAGE',102,.false.,npixb,nlineb,ipb,ierrb)
 
         if (ierrb.eq.0) then
 
*
* OBTAINED SUCCESSFULLY...EXTRACT DESCRIPTOR ITEMS
*
            bscale=1.0
            bzero=0.0
            invalb=-100000
            call gtdscr('BIMAGE','BSCALE','REAL',ival,bscale,cval,ierr)
            call gtdscr('BIMAGE','BZERO','REAL',ival,bzero,cval,ierr)
            call gtdscr('BIMAGE','INVAL','INTEGER',invalb,rval,cval
     :       ,ierr)
*
* OBTAIN USER INTERACTION LEVEL
*
            ilevel=2
            call getpar('ILEVEL','INTEGER',1,1.0,3.0,.true.,ilevel,rval
     :       ,ierr)
 
*
* OBTAIN PERCENTAGE HISTOGRAM RANGE FOR SCALING THE BINNING OF
* IMAGE A
*
            pcrange(1)=2.0
            pcrange(2)=98.0
67          call rdkeyr('PCRANGE',.true.,2,pcrange,nval,istat)
 
            if(max(pcrange(1),pcrange(2)).gt.100.0.or.min(pcrange(1)
     :       ,pcrange(2)).lt.0.0) then
               call wrerr('BADVALUE')
               call rngerr('***REAL VALUES','REAL',0.0,100.0)
               call cnpar('PCRANGE',istat)
               go to 67
 
            endif
 
            pcrange(1)=pcrange(1)*0.01
            pcrange(2)=pcrange(2)*0.01
 
*
* IF THE SCALE FACTOR FOR A IS NEGATIVE, THE IMAGE INTEGERS ARE
* UPSIDE DOWN...INVERT HISTOGRAM RANGE
*
 
            if(ascale.lt.0.0) then
               pcrange(1)=1.0-pcrange(1)
               pcrange(2)=1.0-pcrange(2)
            endif
 
 
*
* CALL PCHIST TO AUTO SCALE, GIVING THE DEFAULT DATA RANGE TO USE
* FOR IMAGE A
*
            irange(1)=minint
            irange(2)=maxint
            call pchist(%val(ipa),npixa,nlinea,invala,pcrange,irange
     :       ,2,ihist,minint,maxint,ierr)
 
*
* IF THE IMAGE HAS NO VALID PIXELS, ABORT WITH ERROR MESSAGE
*
 
            if (ierr.ne.0) then
               call wrerr('NONEVAL')
               go to 99
 
            endif
 
            arange(1)=irange(1)*ascale+azero
            arange(2)=irange(2)*ascale+azero
 
*
* OBTAIN REQUIRED RANGE FROM ENVIRONMENT
*
            call rdkeyr('ARANGE',.true.,2,arange,nval,istat)
 
*
* OBTAIN NUMBER OF BINS FOR BINNING THE SCATTER PLOT
*
            nbin=50
            call getpar('NBIN','INTEGER',1,2.0,32767.0,.true.,nbin,rval
     :       ,ierr)
 
*
* OBTAIN THE NUMBER OF DATA REJECTION ITERATIONS
*
            niter=2
            call getpar('NITER','INTEGER',1,0.0,100.0,.true.,niter,rval
     :       ,ierr)
 
*
* OBTAIN REJECTION THRESHOLD
*
            nsigma=3.0
            call getpar('NSIGMA','REAL',1,0.1,1.0e6,.true.,ival,nsigma
     :       ,ierr)
 
*
* OBTAIN THE MINIMUM NUMBER OF PIXELS REQUIRED PER BIN
*
            minpix=2
            call getpar('MINPIX','INTEGER',1,1.0,1.0e8,.true.,minpix
     :       ,rval,ierr)
 
*
* OBTAIN PLOTTING DEVICE OPTIONS
*
            call defdev(device)
            call getdev('DEVICE',device,.true.,ierr)
            if(ierr.ne.0) goto 99 
 
*
* GENERATE DEFAULT GRAPH HEADINGS
*
            ttl='NORMALIZATION PLOT'

            ttlx='INTENSITY IN IMAGE A ( '
            call gtinam('AIMAGE',ttlx(24:),ierr)
            if(ierr.eq.0) then
               call upperc(ttlx)
               ibdf=index(ttlx,'.BDF')
               if(ibdf.ne.0) ttlx(ibdf:)=' '
               ttlx=ttlx(:lenstr(ttlx))//' )'
            else
               ttlx='INTENSITY IN IMAGE A'
            endif

            ttly='INTENSITY IN IMAGE B ( '
            call gtinam('BIMAGE',ttly(24:),ierr)
            if(ierr.eq.0) then
               call upperc(ttly)
               ibdf=index(ttly,'.BDF')
               if(ibdf.ne.0) ttly(ibdf:)=' '
               ttly=ttly(:lenstr(ttly))//' )'
            else
               ttly='INTENSITY IN IMAGE B'
            endif

*
* OBTAIN NEW HEADINGS IF REQUIRED
*
            if(device.ne.'NONE') then
               call rdkeyc('HEADING',.true.,1,ttl,nval,istat)
               call rdkeyc('XHEAD',.true.,1,ttlx,nval,istat)
               call rdkeyc('YHEAD',.true.,1,ttly,nval,istat)
            endif
 
*
* DETERMINE IF B IMAGE IS TO AUTOMATICALLY NORMALIZED
*
            bnorm(1)=.true.
            call rdkeyl('BNORM',.true.,1,bnorm,nval,istat)
 
*
* OBTAIN WORKSPACE
*
            call getdyn('W1',104,nbin,ip1,ie1)
            call getdyn('W2',204,nbin,ip2,ie2)
            call getdyn('W3',204,nbin,ip3,ie3)
            call getdyn('W4',208,nbin,ip4,ie4)
            call getdyn('W5',204,nbin,ip5,ie5)
 
*
* IF SPACE NOT AVAILABLE, ABORT WITH ERROR MESSAGE
*
 
            if(max(ie1,ie2,ie3,ie4,ie5).ne.0) then
               call wrerr('NOSPACE')
               go to 99
 
            endif
 
 
*
* CALL NMPLOT TO PERFORM NORMALIZATION OF IMAGE B TO IMAGE A
*
            call nmplot(%val(ipa),npixa,nlinea,invala,ascale,azero,
     :      %val(ipb),npixb,nlineb,invalb,bscale,bzero,ilevel,arange(1)
     :       ,arange(2),nbin,niter,nsigma,minpix,device,e(1),c(1),ierrn
     :        ,%val(ip1),%val(ip2),%val(ip3),%val(ip4),%val(ip5),ttl
     :         ,ttlx,ttly)
 
 
*
* IF NO FIT WAS OBTAINED, QUIT WITH ERROR MESSAGE
*
 
            if (ierrn.ne.0) then
               call wrerr('NOFIT')
               go to 99
 
            endif
 
 
*
* WRITE FIT PARAMETERS TO ENVIRONMENT
*
            call wrkeyr('E',e,1,istat)
            call wrkeyr('C',c,1,istat)
 
*
* INSERT NEW NORMALIZED SCALE AND ZERO INTO IMAGE B DESCRIPTOR
*
 
            if(bnorm(1))then
 
               if(abs(e(1)).lt.1.0e-20)e(1)=1.0e-20
               bscale=bscale/e(1)
               bzero=(bzero-c(1))/e(1)
               call ptdscr('BIMAGE','BSCALE','REAL',ival,bscale,cval
     :          ,ierr)
               call ptdscr('BIMAGE','BZERO','REAL',ival,bzero,cval,
     :         ierr)
            endif
 
         endif
 
      endif
 
 
*
* FREE DATA AREAS
*
99    call frdata(' ',istat)
 
*
* RETURN
*
      return
 
      end
 
 
 
