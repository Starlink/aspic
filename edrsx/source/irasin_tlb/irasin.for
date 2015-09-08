      subroutine irasin
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To produce an IPMAF-like image from an EDRS image. The
*       output image is then suitable for use with I_CONTOUR, etc.
*
*SOURCE
*       IRASIN.FOR in IRASIN.TLB
*
*METHOD
*       The output image is in I*4 format with BZERO=0 and BLANK=-999999
*       All EDRS specific descriptors are removed. CROTA2 is removed.
*       If the INSTRUME descriptor indicates that the input image was 
*       not derived from an original image produced by IPMAF software,
*       then INSTRUME is modifed to suggest that the image was created
*       by I_CRDIMAGE (done by prefixing the INSTRUME value with the 
*       string "SURVEY"). This has to be done otherwise IPMAF software
*       will reject the image because of the non-standard INSTRUME 
*       value. NOTE, I_COMBINE and I_CRDIMAGE have different conventions
*       regarding the CROTA1 descriptor. EDRSX software generates images
*       using the I_CRDIMAGE convention. This is why the INSTRUME 
*       descriptor must be set to suggest the image was created by 
*       I_CRIDMAGE, rather than I_COMBINE.
*
*ARGUMENTS
*       none
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               ptderr
*       THIS PACKAGE (IRASIN.TLB):
*               edtoir
*       EDRS:
*               gt2dir,gt2diw,gtdscr,ptdscr,lbgone
*       INTERIM:
*               cydscr,dldscr,frdata,rddscr
*
*STARLINK PARAMETERS
*       INPUT/read/     Input EDRS image
*       OUTPUT/write/   Output PICV image
*
*VAX SPECIFICS
*       implicit none
*       %val
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 28/10/87
*-------------------------------------------------------------------
*
      implicit none

*
* DECLARE LOCAL VARIABLES
*
      integer   blank   ! Value of the BLANK descriptor in the output
      real      bscale  ! Scale factor for input data
      real      bzero   ! Zero offset for input data
      character cval*1  ! Dummy character argument
      integer   ierr    ! Error status
      character instrm*80 ! Value of INSTRUME descriptor from INPUT
      integer   inval   ! The invalid pixel value in input
      integer   ipin    ! Pointer to start of input image
      integer   ipout   ! Pointer to start of output image
      integer   ival    ! Dummy integer argument
      integer   nlin    ! No. of lines in image
      integer   npix    ! No. of pixels in each line of image
      real      rval    ! Dummy real argument
      integer   ustrln  ! Function giving used length of a string.

*
* GET INPUT IMAGE AND EDRS DESCRIPTORS
*
      call gt2dir('INPUT',102,.false.,npix,nlin,ipin,ierr)
      if(ierr.ne.0) goto 999
      call gtdscr('INPUT','INVAL','INTEGER',inval,rval,cval,ierr)
      if(ierr.ne.0) then
         call ptderr('INVAL')
         goto 999
      endif
      call gtdscr('INPUT','BSCALE','REAL',ival,bscale,cval,ierr)
      if(ierr.ne.0) then
         call ptderr('BSCALE')
         goto 999
      endif
      call gtdscr('INPUT','BZERO','REAL',ival,bzero,cval,ierr)
      if(ierr.ne.0) then
         call ptderr('BZERO')
         goto 999
      endif

*
* GET THE OUTPUT IMAGE
*
      call gt2diw('OUTPUT',104,.false.,npix,nlin,ipout,ierr)
      if(ierr.ne.0) goto 999

*
* CALL EDTOIR TO PRODUCE THE I*4 IMAGE FROM THE I*2 IMAGE, SETTING
* INVALID PIXELS TO THE BLANK VALUE OF -999999
*
      blank=-999999
      call edtoir(%val(ipin),%val(ipout),npix,nlin,inval,blank,bscale,
     :            bzero)

*
* COPY ALL DESCRIPTORS FROM THE INPUT TO THE OUTPUT
*
      call cydscr('INPUT','OUTPUT',ierr)

*
* THE INSTRUME DESCRIPTOR DETERMINES HOW THE IMAGE WAS CREATED...(NOTE,
* EDRS ROUTINE GTDSCR ONLY RETURNS THE FIRST 30 CHARACTERS OF A 
* CHARACTER DESCRIPTOR. ALL 80 ARE NEEDED FOR INSTRUME. THEREFORE
* ROUTINE RDDSCR IS USED INSTEAD).
*
      call rddscr('INPUT','INSTRUME',1,instrm,ival,ierr)
      if(ierr.ne.0) then
         call ptderr('INSTRUME')
      endif

*
* IF THE IMAGE WAS CREATED WITHIN EDRSX (EG BY MEMCRDD OR IF DESCRIPTORS
* WERE ADDED BY IRASDSCR ) THEN THE INSTRUME VALUE WILL PROBABLY BE
* UNRECOGNISED BY IPMAF SOFTWARE. IF THE INSTRUME VALUE IS NON-STANDARD,
* PREFIX IT WITH "SURVEY" TO MAKE IPMAF TREAT IT LIKE AN IMAGE CREATED
* BY I_CRDIMAGE. (FIRST REMOVE LEADING BLANKS AND ANY LEADING 
* APOSTROPHE).
*
      call lbgone(instrm)
      if( instrm(1:1) .eq. '''' ) then
         instrm(1:1) = ' '
         call lbgone(instrm)
      end if

      if(index(instrm,'COMBINE').ne.1 .and.
     :   index(instrm,'CPC').ne.1 .and. 
     :   index(instrm,'SUR').ne.1 .and. 
     :   index(instrm,'DEEP').ne.1 .and. 
     :   index(instrm,'GALPL').ne.1 .and. 
     :   index(instrm,'IRAS').eq.0 ) then

         call ptdscr('OUTPUT','INSTRUME','CHARACTER',ival,rval,
     :               ' SURVEY '//instrm(:ustrln(instrm)),ierr)

      endif

*
* UPDATE BSCALE AND BZERO IN OUTPUT
*
      call ptdscr('OUTPUT','BSCALE','REAL',ival,bscale,cval,ierr)
      call ptdscr('OUTPUT','BZERO','REAL',ival,bzero,cval,ierr)

*
* ADD DESCRIPTOR BLANK
*
      call ptdscr('OUTPUT','BLANK','INTEGER',blank,rval,cval,ierr)

*
* SET BITPIX TO 32
*
      call ptdscr('OUTPUT','BITPIX','INTEGER',32,rval,cval,ierr)

*
* REMOVE CROTA2 DESCRIPTOR
*
      call dldscr('OUTPUT','CROTA2',ierr)

*
* REMOVE EDRS DESCRIPTORS
*
      call dldscr('OUTPUT','INVAL',ierr)
      call dldscr('OUTPUT','TITLE',ierr)

*
* FREE DATA AREAS AND FINISH
*
  999 call frdata(' ',ierr)

      end
