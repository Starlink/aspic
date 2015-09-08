      subroutine edrsin
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To convert a non-EDRS format file to an EDRS format file,
*       taking account of FITS descriptors BSCALE, BZERO, BLANK and
*       OBJECT. The input frame can be of any of the 7 suported
*       Starlink data types.
*
*SOURCE
*       EDRSIN.FOR in EDRSIN.TLB
*
*METHOD
*       Low level routines from the INTERIM library are used to determine
*       what incarnations are present in the input image. If more than one
*       incarnation exists then the user is asked to choose which one is
*       to be used for the conversion. There is a seperate conversion
*       routine for each possible input data format.
*             All descriptors in the input are passed to the output. Any
*       descriptors present in the input which are used by EDRS are then
*       updated any which are not present are added to the output image.
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               GTFMTS,RESCL1,RESCL2,RESCL3,RESCL4,RESCL5,RESCL6,RESCL7
*               GTSTRN
*       EDRS:
*               GT2DIR,GTDSCR,GT2DIW,PTSTDS,PTDSCR
*       INTERIM:
*               WRUSER,WRERR,CYDSCR,RDKEYC,FRDATA
*
*STARLINK PARAMETERS
*       DTYPE/read/     The input data type to be used for conversion
*       ILEVEL/read/    The user information display level
*       INPUT/read/     The input image
*       OUTPUT/write/   The output image
*       TITLE/read/     A title for the output image
*       NOINCARN/error/ Accessed if the input file contains no incarnations
*       NOVALID/error/  Accessed if the input file contains no valid pixels
*       TOOBAD/error/   Accessed if too many bad parameter values are given
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB)  26/8/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE LOCAL VARIABLES
*
      logical   allrej          ! True if input image contains no valid pixels
      character alts*80         ! List of format specifiers available to user
      real      blank           ! Value of blank pixels in input
      real      bscale          ! Scale factor for input data values
      real      bzero           ! Zero level for input data values
      character cval*1          ! Dummy character argument
      character fmtnam(7)*17    ! Descriptions of the recognized formats
      character fmtspc(7)*2     ! Strings for specifying formats to user
      logical   formts(7)       ! Each flag is set true if corresponding
                                ! incarnation exists in the input image
      integer   i               ! Loop count
      integer   ialt            ! Pointer to start of next specifier in alts
      integer   ierr            ! Status return value
      integer   ifmts(7)        ! The recognized formats codes
      integer   ilevel          ! Amount of information to display on screen
      integer   iorig           ! Selected input data type in range 1 to 7
      character iname*2         ! Format specifier selected by user
      integer   inval           ! Value of invalid pixels in output
      integer   ipin            ! Pointer to start of image data area
      integer   ipout           ! Pointer to start of output data area
      integer   itype           ! The position of the selected format specifier
                                ! within list of alternatives
      integer   ival            ! Dummy integer argument
      integer   nchar           ! No. of characters in value returned by GTSTRN
      integer   nfmt            ! No. of incarnations in the input image
      integer   nlin            ! No. of lines in input image
      integer   npix            ! No. of pixels in input image
      logical   pinval          ! True if descriptor INVAL is present in input
      character prbuf*80        ! Buffer for output to screen
      real      rval            ! Dummy real argument
      character title(1)*30     ! Title for output image
*
* INITIALIZE FORMAT DATA (SEE SUN4)
*
      data      ifmts/
     :                    101,
     :                    102,
     :                    104,
     :                    204,
     :                    208,
     :                    301,
     :                    302/,
     :          fmtnam/
     :                    'Signed byte',
     :                    'Signed word',
     :                    'Signed longword',
     :                    'Real',
     :                    'Double precision',
     :                    'Unsigned byte',
     :                    'Unsigned word'/,
     :          fmtspc/
     :                    'SB',
     :                    'SW',
     :                    'SL',
     :                    'R',
     :                    'DP',
     :                    'UB',
     :                    'UW'/
*
* CALL GTFMTS TO FIND OUT THE DATA TYPES CORRESPONDING TO THE
* INCARNATIONS OF THE INPUT DATA FRAME
*
      call gtfmts('INPUT',formts,ierr)
      if(ierr.ne.0) call wrerr('TOOBAD')
      if(ierr.eq.0) then
*
* GET INFORMATION DISPLAY LEVEL
*
         call getpar('ILEVEL','INTEGER',1,1.0,3.0,.false.,ilevel,
     :               rval,ierr)
         if(ierr.ne.0) then
            call wrerr('TOOBAD')
            goto 999
         endif
*
* FIND OUT HOW MANY INCARNATIONS THERE ARE IN THE INPUT DATA FRAME
*
         nfmt=0
         do i=1,7
            if(formts(i)) nfmt=nfmt+1
         enddo
*
* IF THERE ARE MORE THAN 1 INCARNATION IN THE INPUT, THEN ASK THE
* USER WHICH INCARNATION IS TO BE USED FOR THE CONVERSION
*
         if(nfmt.gt.1) then
*
* UNLESS ILEVEL WAS GIVEN AS 1 DISPLAY OPTIONS ON SCREEN
*
            if(ilevel.gt.1) then
               call wruser(' ',ierr)
               call wruser('Incarnations of the input exist with the '//
     :                     'following data types:',ierr)
               call wruser(' ',ierr)
            endif
            alts=' '
            ialt=1
            do i=1,7
               if(formts(i)) then
                  if(ilevel.gt.1) then
                     write(prbuf,10) fmtnam(i),fmtspc(i)
   10                format('   ',A17,' ( specify as "',A2,'" )')
                     call wruser(prbuf,ierr)
                  endif
                  alts(ialt:ialt+2)=fmtspc(i)//','
                  ialt=ialt+3
               endif
            enddo
            alts(ialt-1:ialt-1)='.'
*
* ASK USER FOR REQUIRED FORMAT SPECIFIER
*
            if(ilevel.gt.1) call wruser(' ',ierr)
            itype=1
            call gtstrn('DTYPE',.true.,alts,1,itype,iname,nchar,
     :                   ierr)
            if(ierr.ne.0) then
               call wrerr('TOOBAD')
               goto 999
            endif
            iorig=1
            do while(fmtspc(iorig).ne.iname.and.iorig.le.7)
               iorig=iorig+1
            enddo
*
* IF THERE WAS ONLY 1 INCARNATION IN THE INPUT, FIND THE INDEX OF ITS
* DATA TYPE
*
         else if(nfmt.eq.1) then
            iorig=0
            do i=1,7
               if(formts(i)) iorig=i
            enddo
*
* IF THERE WERE NO INCARNATIONS THEN GIVE ERROR MESSAGE AND STOP
*
         else
            if(iorig.eq.0) then
               call wrerr('NOINCARN')
               goto 999
            endif
         endif
*
* GET INPUT FRAME WITH THE SELECTED DATA TYPE
*
         call gt2dir('INPUT',ifmts(iorig),.false.,npix,nlin,ipin,ierr)
         if(ierr.eq.0) then
*
* SET UP DEFAULT FITS DESCRIPTORS AND THEN GET VALUES FROM INPUT FRAME
*
            bscale=1.0
            bzero=0.0
            pinval=.true.
            call gtdscr('INPUT','BSCALE','REAL',ival,bscale,cval,ierr)
            call gtdscr('INPUT','BZERO','REAL',ival,bzero,cval,ierr)
            call gtdscr('INPUT','BLANK','REAL',ival,blank,cval,ierr)
*
* IF 'BLANK' WAS NOT FOUND THEN THERE ARE NO INVALID PIXELS
*
            if(ierr.ne.0) pinval=.false.
            call gtdscr('INPUT','OBJECT','CHARACTER',ival,rval,
     :                   title(1),ierr)
*
* IF 'OBJECT' WAS NOT FOUND SET TITLE TO NULL
*
            if(ierr.ne.0) title(1)=' '
*
* GET OUTPUT FRAME WITH I*2 FORMAT
*
            call gt2diw('OUTPUT',102,.false.,npix,nlin,ipout,ierr)
            if(ierr.ne.0) call wrerr('TOOBAD')
            if(ierr.eq.0) then
*
* CALL THE RELEVANT ROUTINE TO CONVERT BETWEEN THE INPUT DATA TYPE AND
* THE OUTPUT DATA TYPE (I*2)
*
               if(iorig.eq.1) then
                  call rescl1(%val(ipin),npix,nlin,bscale,bzero,blank,
     :                        %val(ipout),pinval,allrej)
               else if(iorig.eq.2) then

                  call rescl2(%val(ipin),npix,nlin,bscale,bzero,blank,
     :                        %val(ipout),pinval,allrej)
               else if(iorig.eq.3) then
                  call rescl3(%val(ipin),npix,nlin,bscale,bzero,blank,
     :                        %val(ipout),pinval,allrej)
               else if(iorig.eq.4) then
                  call rescl4(%val(ipin),npix,nlin,bscale,bzero,blank,
     :                        %val(ipout),pinval,allrej)
               else if(iorig.eq.5) then
                  call rescl5(%val(ipin),npix,nlin,bscale,bzero,blank,
     :                        %val(ipout),pinval,allrej)
               else if(iorig.eq.6) then
                  call rescl6(%val(ipin),npix,nlin,bscale,bzero,blank,
     :                        %val(ipout),pinval,allrej)
               else if(iorig.eq.7) then
                  call rescl7(%val(ipin),npix,nlin,bscale,bzero,blank,
     :                        %val(ipout),pinval,allrej)
               endif
*
* IF THERE WERE NO VALID PIXELS IN THE INPUT THEN GIVE A MESSAGE
*
               if(allrej)  call wrerr('NOVALID')
*
* COPY THE INPUT DESCRIPTOR TO THE OUTPUT AND UPDATE CHANGED ENTRIES
*
               call cydscr('INPUT','OUTPUT',ierr)
               call rdkeyc('TITLE',.true.,1,title,ival,ierr)
               inval=blank
               call ptstds('OUTPUT',npix,nlin,1,inval,bscale,bzero,
     :                      title)
               call ptdscr('OUTPUT','BITPIX','INTEGER',16,rval,cval,
     :                      ierr)
               call ptdscr('OUTPUT','BLANK','INTEGER',inval,rval,cval,
     :                      ierr)
            endif
         endif
      endif
*
* FREE DATA AREAS AND FINISH
*
  999 call frdata(' ',ierr)
      end
