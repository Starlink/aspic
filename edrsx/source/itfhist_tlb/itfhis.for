      subroutine itfhis
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Generates an Intensity Transfer Function table which
*       causes the histogram of an image corrected with the ITF,
*       to match the histogram of a reference image.
*
*SOURCE
*       ITFHIS.FOR in ITFHIST.TLB
*
*METHOD
*       Two inputs are required, each of which can be either an image,
*       or a histogram of an image (histograms are recognised by only
*       having 1 line and having descriptors XSCALE and XZERO). If
*       images are supplied, their histograms are calculated by calls
*       to PCHIST. These two inputs are refered to as the "data" input
*       (INPUTA) and the "reference" input (INPUTB). The ITF generated
*       will cause the "data" image to have a histogram which matches
*       that of the "reference" image. If a data image is actually given
*       (rather than its histogram), then an output image can optionally
*       be generated holding the result of applying the ITF to the data
*       image. Also the ITF itself can optionally be output as an EDRS
*       ITF frame in a form which can be used as input to EDRS programs
*       ITFPLOT and ITFCORR.
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              gt2dir,gt2diw,wrerr,gtwork,i2toi4
*       THIS PACKAGE (ITFHIST.TLB):
*              calitf
*       EDRS:
*              gtstds,pchist,gtdscr,ptdscr,lncorn
*       INTERIM:
*              rdkeyc,cydscr,frdata
*STARLINK PARAMETERS
*       INPUTA/read/    The data input, image or histogram
*       INPUTB/read/    The reference input, image or histogram
*       ITF/read/       The output frame to hold the ITF
*       ITITLE/read/    A title for the ITF frame
*       OUTPUT/read/    The output corrected image, if required
*       TITLE/read/     A title for the output corrected image
*       TOOBAD/error/   Accessed if too many bad parameter values given
*       NOVALID/error/  Accessed if an input image had no valid data
*
*VAX SPECIFICS
*       implicit none
*       %val
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 27/5/88
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE LOCAL VARIABLES
*
      integer   ainval  ! Invalid pixel value in data input
      character atitle*80! Title from data image
      real      ascale  ! Scale factor for data input pixel values
      integer   axhi    ! Upper limit of pixel locations in histogram A
      integer   axlo    ! Lower limit of pixel locations in histogram A
      real      axscl   ! Scale factor for pixel LOCATIONS in histgrm A
      real      axzer   ! Zero offset for pixel LOCATIONS in histgrm A
      real      azero   ! Zero offset for data image pixel values
      integer   binval  ! Invalid pixel value in reference input
      real      botlim  ! Lower data limit of ITF table
      real      bscale  ! Scale factor for reference image data values
      character btitle*80! Title from reference input
      integer   bxhi    ! Upper limit of pixel locations in histogram B
      integer   bxlo    ! Lower limit of pixel locations in histogram B
      real      bxscl   ! Scale factor for pixel LOCATIONS in histgrm B
      real      bxzer   ! Zero offset for pixel LOCATIONS in histgrm B
      real      bzero   ! Zero offset for reference image data values
      character cval*1  ! Dummy character argument
      logical   dathis  ! True if data input was a histogram
      integer   ierr    ! Error status
      integer   ival    ! Dummy integer argument
      integer   ipcha   ! Pointer to data cumulative histogram
      integer   ipchb   ! Pointer to reference cumulative histogram
      integer   ipha    ! Pointer to histogram of data image
      integer   iphb    ! Pointer to histogram of reference image
      integer   ipina   ! Pointer to data input image
      integer   ipinb   ! Pointer to reference input image
      integer   ipitf   ! Pointer to ITF table
      integer   ipout   ! Pointer to output corrected image
      character ititle*80! Title for ITF output frame
      integer   maxint  ! Maximum integer value in input frames
      integer   minint  ! Minimum integer value in input frames
      integer   nentry  ! No. of entries in final ITF table
      integer   nlina   ! No. of lines in data input
      integer   nlinb   ! No. of lines in reference input
      integer   npixa   ! No. of pixels per line in data input
      integer   npixb   ! No. of pixels per line in reference input
      real      rval    ! Dummy real argument
      real      scale   ! The scale factor of the output corrected image
      real      toplim  ! Upper data limit of ITF table
      real      zero    ! Zero offset of the output corrected image

      parameter (maxint=32767,minint=-32768)
*
* GET INPUT FRAME HOLDING IMAGE TO BE CORRECTED OR ITS HISTOGRAM, AND
* GET THE REQUIRED DESCRIPTORS
*
      call gt2dir('INPUTA',102,.false.,npixa,nlina,ipina,ierr)
      if(ierr.ne.0) then
         call wrerr('TOOBAD')
         goto 999
      endif
      call gtstds('INPUTA',1,ainval,ascale,azero,atitle)
*
* HISTOGRAMS HAVE EXTRA DESCRIPTORS XSCALE AND XZERO RELATING PIXEL
* LOCATION TO DATA VALUE, I.E. DATA VALUE=XSCALE*(PIXEL NO.)+XZERO
*
      call gtdscr('INPUTA','XSCALE','REAL',ival,axscl,cval,ierr)
      if(ierr.eq.0) call gtdscr('INPUTA','XZERO','REAL',ival,axzer,
     :                           cval,ierr)
*
* CALCULATE HISTOGRAM OF DATA IMAGE IF INPUT IS NOT A HISTOGRAM ITSELF
*
      if(ierr.ne.0) then
         dathis=.false.
         nentry=maxint-minint+1
         call gtwork('HISTA','INTEGER',nentry,ipha,ierr)
         if(ierr.ne.0) goto 999
         rval=0
         call pchist(%val(ipina),npixa,nlina,ainval,rval,ival,1,
     :               %val(ipha),minint,maxint,ierr)
         axlo=minint
         axhi=maxint
*
* IF INPUT WAS A HISTOGRAM SET FLAG, SET NO. OF ENTRIES IN ITF AND
* CONVERT TO INTEGER ARRAY
*
      else
         dathis=.true.
         nentry=npixa
         call gtwork('HISTA','INTEGER',nentry,ipha,ierr)
         if(ierr.ne.0) goto 999
         call i2toi4(%val(ipina),npixa,1,ascale,azero,ainval,
     :               %val(ipha),0,ierr)
         axlo=1
         axhi=npixa
         ascale=axscl
         azero=axzer
      endif
      if(ierr.ne.0) then
         call wrerr('NOVALID')
         goto 999
      endif
*
* SET UPPER AND LOWER DATA LIMITS OF ITF
*
      botlim=ascale*axlo+azero
      toplim=ascale*axhi+azero
*
* GET INPUT FRAME HOLDING REFERENCE IMAGE OR HISTOGRAM AND GET
* REQUIRED DESCRIPTORS
*
      call gt2dir('INPUTB',102,.false.,npixb,nlinb,ipinb,ierr)
      if(ierr.ne.0) then
         call wrerr('TOOBAD')
         goto 999
      endif
      call gtstds('INPUTB',1,binval,bscale,bzero,btitle)
      call gtdscr('INPUTB','XSCALE','REAL',ival,bxscl,cval,ierr)
      if(ierr.eq.0) call gtdscr('INPUTB','XZERO','REAL',ival,bxzer,
     :                           cval,ierr)
*
* CALCULATE HISTOGRAM OF REFERENCE IMAGE IF INPUT IS NOT A HISTOGRAM
* ITSELF
*
      if(ierr.ne.0) then
         call gtwork('HISTB','INTEGER',nentry,iphb,ierr)
         if(ierr.ne.0) goto 999
         rval=0
         call pchist(%val(ipinb),npixb,nlinb,binval,rval,ival,1,
     :               %val(iphb),minint,maxint,ierr)
         bxlo=minint
         bxhi=maxint
      else
         call gtwork('HISTB','INTEGER',nentry,iphb,ierr)
         if(ierr.ne.0) goto 999
         call i2toi4(%val(ipinb),npixb,1,bscale,bzero,binval,
     :               %val(iphb),0,ierr)
         bscale=bxscl
         bzero=bxzer
         bxlo=1
         bxhi=npixb
      endif
      if(ierr.ne.0) then
         call wrerr('NOVALID')
         goto 999
      endif
*
* GET WORKSPACE FOR CUMULATIVE HISTOGRAMS
*
      call gtwork('CHISTA','INTEGER',axhi-axlo+3,ipcha,ierr)
      if(ierr.eq.0) call gtwork('CHISTB','INTEGER',bxhi-bxlo+3,
     :                           ipchb,ierr)
      if(ierr.ne.0) goto 999
*
* GET OUTPUT FRAME TO HOLD ITF, OR WORKSPACE IF NO OUTPUT REQUIRED
*
      call gt2diw('ITF',204,.true.,nentry,1,ipitf,ierr)
      if(ierr.ne.0) then
         call gtwork('ITF','REAL',nentry,ipitf,ierr)
         if(ierr.ne.0) goto 999
      else
         ititle='ITF TABLE'
         call rdkeyc('ITITLE',.true.,1,ititle,ival,ierr)
         call ptdscr('ITF','TITLE','CHARACTER',ival,rval,ititle,ierr)
         call ptdscr('ITF','UPLIM','REAL',ival,toplim,cval,ierr)
         call ptdscr('ITF','LOLIM','REAL',ival,botlim,cval,ierr)
         call ptdscr('ITF','NAXIS','INTEGER',2,rval,cval,ierr)
         call ptdscr('ITF','NAXIS1','INTEGER',nentry,rval,cval,ierr)
         call ptdscr('ITF','NAXIS2','INTEGER',1,rval,cval,ierr)
      endif
*
* CALCULATE THE ITF BY CALLING CALITF
*
      call calitf(%val(ipha),%val(ipcha),%val(iphb),%val(ipchb),
     :            %val(ipitf),axlo,axhi,bxlo,bxhi,bscale,bzero)
*
* IF REQUIRED APPLY THE CORRECTION TO THE DATA IMAGE AND PRODUCE AN
* OUTPUT IMAGE
*
      if(.not.dathis) then
         call gt2diw('OUTPUT',102,.true.,npixa,nlina,ipout,ierr)
         if(ierr.eq.0) then
            if(ainval.lt.minint.or.ainval.gt.maxint) ainval=-32767
            call lncorn(%val(ipina),npixa,nlina,ainval,ascale,azero,
     :                  botlim,toplim,%val(ipitf),nentry,%val(ipout),
     :                  ainval,scale,zero,ierr)
            call cydscr('INPUTA','OUTPUT',ierr)
            call ptdscr('OUTPUT','BSCALE','REAL',ival,scale,cval,ierr)
            call ptdscr('OUTPUT','BZERO','REAL',ival,zero,cval,ierr)
            call rdkeyc('TITLE',.true.,1,atitle,ival,ierr)
            call ptdscr('OUTPUT','TITLE','CHARACTER',ival,rval,atitle,
     :                  ierr)
            call ptdscr('OUTPUT','INVAL','INTEGER',ainval,rval,cval,
     :                  ierr)
         endif
      endif
*
* RELEASE DATA AREAS AND FINISH
*
 999  call frdata(' ',ierr)

      end
