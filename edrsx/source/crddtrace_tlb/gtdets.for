      subroutine gtdets(name,dets,ballno,ndets,ndtout,ierr)
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To get a list of detectors for display by CRDDTRACE. Each
*       detector is identified by its position across the focal plane
*       ordered by z from 1 to 16,low z to high z (i.e. NOT Ball no.).
*
*SOURCE
*       GTDETS.FOR in CRDDTRACE.TLB
*
*METHOD
*       The user can specify any of the following:
*
*               "ALL"           : All detectors are displayed
*               "<ret>"         : A default list is used
*               " xx,yy,zz,..." : A list of specific detectors are used
*               " xx TO yy"     : All detectors from xx to yy are used
*
*       If for any reason, the first attempt to get a valid list fails,
*       the user is given a list of valid detectors
*
*ARGUMENTS
*   INPUTS:
*       name    character       Parameter name to use for getting
*                               detectors
*       ndets   integer         Max no of detectors allowed (usually 16)
*       ballno(ndets) integer   A list of Ball numbers corresponding to
*                               the 16 detectors order from low to high
*                               focal plane z position.
*       ndtout  integer         The default no. of detectors
*   OUTPUTS:
*       dets(ndets)   integer   Array holding selected detector
*                               identifiers in range 1 to 16 (low to
*                               high focal plane z positions)
*       ndtout  integer         The number of detectors to use
*       ierr    integer         Status value: 0 - Success
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               wrerr,bblsrt,swapi,strlen
*       INTERIM:
*               wruser,rdkeyc,ctoi,
*
*STARLINK PARAMETERS
*       'name'/read/    The argument name contains the parameter name
*                       used to aquire the list of detectors.
*       TOOBAD/error/   Accessed if too many bad lists have been given.
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       variable format expressions
*       end of line comments
*       do while
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 4/9/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE THE ARGUMENTS
*
      integer ndets,dets(ndets),ballno(ndets),ndtout,ierr
      character name*(*)
*
* DECLARE LOCAL VARIABLES
*
      integer           badval          ! A bad Ball no. or array index
      character         cdets(32)*10    ! Character buffer for detector ids
      integer           detbal          ! Detector ball no.
      integer           detbuf(32)      ! Buffer for final detector id.s
      integer           hidet           ! Hi limit of detector id range
      integer           hilim           ! Hi limit of Ball no. range
      integer           i               ! Implied loop count
      integer           idet            ! Detector loop count
      integer           idtout          ! Loop count for displayed detectors
      integer           ival            ! Dummy integer argument
      integer           lodet           ! Low limit of detector id range
      integer           lolim           ! Low limit of Ball no. range
      integer           ltext           ! No. of characters in text
      integer           nbad            ! No. of bad parameter values given
      integer           ndtdef          ! The no. of default detectors
      character*80      prbuf           ! Buffer for text sent to user terminal
      real              rval            ! Dummy real argument
      integer           strlen          ! Length of string minus trailing blanks
      integer           tostrt          ! Start of string "TO
      character*80      text            ! Temporary text buffer

*
* OBTAIN A LIST OF DETECTOR BALL NO.S FROM THE ENVIRONMENT
*
      ndtdef=ndtout
      nbad=0
 10   call rdkeyc(name,.false.,ndets,cdets,ival,ierr)
*
* SEE IF USER HAS SPECIFIED A RANGE OF DETECTORS ( "xx TO yy" )
*
      tostrt=max(index(cdets(1),'to'),index(cdets(1),'TO'))
*
* IF NULL RETURN WAS MADE BY USER WHEN PROMPTED, ACCEPT THE DEFAULT LIST
*
      if(ierr.eq.1) then
         ierr=0
         ndtout=ndtdef
*
* IF AN ERROR OCCURED READING THE VALUE FROM THE USER, SET THE 'UNKNOWN'
* TEMPORARY ERROR CONDITION
*
      else if(ierr.ne.0) then
         ierr=999
*
* IF USER SPECIFIED A RANGE THEN....
*
      else if(tostrt.gt.0) then
*
* GET THE LOWER DETECTOR BALL NO, OR USE THE LOWEST DETECOR IF NON
* WAS SPECIFIED
*
         if(tostrt.gt.1) then
            call ctoi(cdets(1)(:tostrt-1),lolim,ierr)
            if(ierr.ne.0) then
               ierr=2
               badval=1
            endif
         else
            lolim=ballno(1)
         endif
*
* THEN GET THE UPPER DETECTOR BALL NO, ASSUMING HIGHEST DETECTOR IF NON
* GIVEN
*
         if(ierr.eq.0) then
            if(strlen(cdets(1)).ge.tostrt+2) then
               call ctoi(cdets(1)(tostrt+2:),hilim,ierr)
               if(ierr.ne.0) then
                  ierr=2
                  badval=1
               endif
            else
               hilim=ballno(ndets)
            endif
         endif
*
* NOW FIND THE CROSS SCAN POSITION OF THE UPPER AND LOWER DETECTORS
*
         if(ierr.eq.0) then
            lodet=0
            hidet=0
            do idtout=1,ndets
               if(ballno(idtout).eq.lolim) lodet=idtout
               if(ballno(idtout).eq.hilim) hidet=idtout
            enddo
*
* IF EITHER OF THE DETECTORS SPECIFIED ARE NOT IN THE CURRENT BAND, THEN
* SET THE CORRESPONDING TEMPORARY ERROR CONDITION
*
            if(lodet.eq.0) then
               ierr=3
               badval=lolim
            else if(hidet.eq.0) then
               ierr=3
               badval=hilim
*
* IF THE DETECTORS WERE GIVEN IN THE WRONG ORDER, SWAP THEM ROUND
*
            else
               if(hidet.lt.lodet) call swapi(hidet,lodet)
*
* INCLUDE ALL DETECTORS FROM LODET TO HIDET IN THE OUTPUT LIST
*
               ndtout=hidet-lodet+1
               do idtout=1,ndtout
                  dets(idtout)=idtout+lodet-1
               enddo
            endif
         endif
*
* IF 'ALL' WAS SPECIFIED ACCEPT ALL DETECTORS
*
      else if(max(index(cdets(1),'ALL'),index(cdets(1),'all')).ne.0)
     :                                                              then
         do idtout=1,ndets
            dets(idtout)=idtout
         enddo
         ndtout=ndets
*
* OTHERWISE CONVERT REQUESTED BALL NO.S TO DETECTOR IDENTIFIERS
* (ORDERED BY DETECTOR Z POSITION)
*
      else
         ndtout=ival
         do idtout=1,ndtout
            if(ierr.eq.0) then
               call ctoi(cdets(idtout),detbal,ierr)
               if(ierr.eq.0) then
*
* IDENTIFY THE VALUE WITHIN THE LIST OF BALL NUMBERS
* FOR THIS BAND ORDERED BY FOCAL PLANE Z
*
                  idet=1
                  do while(detbal.ne.ballno(idet).and.idet.le.ndets)
                    idet=idet+1
                  enddo
                  if(idet.le.ndets) then
                     detbuf(idtout)=idet
*
* IF DETECTOR IS NOT IN CURRENT BAND, SET CORRESPONDING TEMPORARY ERROR
* CONDITION
*
                  else
                     ierr=3
                     badval=detbal
                  endif
               else
*
* IF AN ERROR OCCURED READING THE DETECTOR IDENTIFIER TO INTEGER, SET
* CORRESPONDONG TEMPORARY ERROR CONDITION
*
                  ierr=2
                  badval=idtout
               endif
            endif
         enddo
*
* IF ALL DETECTORS WERE SPECIFIED CORRECTLY, COPY THEM TO THE OUTPUT
* AND ORDER THEM INTO CROSS SCAN POSITION
*
         if(ierr.eq.0) then
            do idet=1,ndtout
               dets(idet)=detbuf(idet)
            enddo
            call bblsrt(dets,ndets,ndtout)
         endif

      endif
*
* IF FOR ANY REASON NO LIST OF DETECTORS WAS OBTAINED, DECIDE WAHT TO DO
*
      if(ierr.ne.0) then
*
* IF USER HAS GIVEN 3 INVALID LISTS, GIVE UP
*
         if(nbad.gt.3) then
            call wrerr('TOOBAD')
            ierr=16

         else
*
* SET MESSAGE IF ANY NON-NUMERIC VALUES WERE GIVEN
*
            if(ierr.eq.2) then
               write(prbuf,31) cdets(badval)
  31           format('   *** BAD VALUE:',A5)
*
* SET MESSAGE IF ANY OF THE DETECTORS REQUESTED ARN'T IN THE INPUT DATA
*
            else if(ierr.eq.3) then
               write(prbuf,41) badval
  41           format('   *** Detector ',I2,' is not ',
     :                'available. Re-enter list of',
     :                ' detectors')
*
* SET MESSAGE FOR UNKNOWN ERRORS
*
            else
               prbuf='*** Unable to get valid list of detectors'
            endif
*
* WRITE OUT THE MESSAGE
*
            call wruser(' ',ierr)
            call wruser(prbuf,ierr)
            call wruser(' ',ierr)
*
* LIST THE DETECTOR BALL NO.S FOR THE CURRENTLY SELECTED
* BAND AND A LIST OF DEFAULT DETECTOR BALL NUMBERS.
*
            write(prbuf,11) (ballno(i),i=1,ndets)
  11        format('   ',<ndets-1>(I2,', '),I2)
            call wruser(' The following detectors are available :',
     :                   ierr)
            call wruser(prbuf,ierr)
            call wruser(' ',ierr)
            call wruser(' Enter a list of the detectors you '//
     :                  'wish to plot',ierr)
            call wruser(' or press <RETURN> to use the following '//
     :                  'default list',ierr)
            write(prbuf,21) (ballno(dets(i)),i=1,ndtdef)
  21        format('   ',<ndtdef>(I2,', '))
            call wruser(prbuf(:strlen(prbuf)-1),ierr)
*
* INCREMENT THE NUMBER OF BAD LISTS GIVEN AND GET A NEW LIST FROM USER
*
            nbad=nbad+1
            call cnpar(name,ierr)
            goto 10
         endif
      endif
*
* FINISH
*
 999  continue

      end
