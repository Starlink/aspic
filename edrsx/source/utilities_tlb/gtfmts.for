      subroutine gtfmts(name,formats,ierr)
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO OBTAIN THE DATA TYPES OF ALL INCARNATIONS PRESENT IN A
*       DATA FRAME
*
*SOURCE
*       GTFMTS.FOR in UTILITIES.TLB
*
*ARGUMENTS
*       NAME (IN)
*       CHARACTER*(*)
*               THE PARAMETER NAME USED TO ACCESS THE INPUT FRAME
*       FORMATS (OUT)
*       LOGICAL(7)
*               A FLAG FOR EACH FORMAT SET TO .TRUE. IF PRESENT, .FALSE.
*               OTHERWISE, IN ORDER OF:
*                       SB,SW,SL,R,DP,UB,UW
*       IERR (OUT)
*       INTEGER
*               ERROR FLAG: ZERO FOR SUCCESS
*
*USED BY
*       EDRSIN
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               RDFORM
*       EDRS:
*               WRERR
*       INTERIM:
*               CNPAR,WRUSER
*
*STARLINK PARAMETERS
*       'NAME'/read/    The parameter name for the input image is given in the
*                       argument 'name'
*       NONULL/error/   Accessed if a null entry is given
*       NOACCESS/error/ Accessed if the frame cannot be accessed
*       DIMINVAL/error/ Accessed if the image dimensions are invalid
*       NOTIMAGE/error/ Accessed if the frame does not contain an image
*
*AUTHOR
*       D.S. Berry  (MAVAD::DSB) 26/8/87
*       (based on EDRS subroutine GT2DIR by R.F. Warren-Smith)
*-----------------------------------------------------------------------
*
*
      character name*(*)
      logical formats(7),exit
      integer dims(2),cnstat
*
* SET MAXIMUM NUMBER OF BAD ENTRIES TO BE TOLERATED
*
      parameter (maxbad=3)
*
* INITIALLISE BAD ENTRY COUNTER AND OBTAIN REQUIRED IMAGE FRAME INFO
*
      nbad=0
11    call rdform(name,formats,istat)
      ierr=istat
*
* IF NULL ENTERED:
*
      if(istat.eq.1) then
         call wrerr('NONULL')
         exit=.false.
*
* IF FRAME COULD NOT BE ACCESSED, GIVE MESSAGE AND RETURN FOR NEW FRAME
*
      else if(istat.eq.3) then
         call wrerr('NOACCESS')
         exit=.false.
*
* IF PARAMETER NAME IS INVALID, RETURN WITH IERR=4
*
      else if(istat.eq.4) then
         exit=.true.
*
* IF IMAGE HAS INVALID DIMENSIONS, GIVE MESSAGE AND RETURN FOR NEW
* FRAME
*
      else if(istat.eq.6) then
         call wrerr('DIMINVAL')
         exit=.false.
*
* IF FRAME IS NOT AN IMAGE, GIVE MESSAGE AND RETURN FOR NEW FRAME
*
      else if(istat.eq.7) then
         call wrerr('NOTIMAGE')
         exit=.false.
*
* OTHERWISE FRAME IS OK, SO EXIT
*
      else
         exit=.true.
      endif
*
* IF A BAD ENTRY WAS GIVEN AND A NEW ONE IS TO BE OBTAINED,
* CANCEL PREVIOUS ENTRY AND COUNT THE NUMBER OF BAD ENTRIES
*
      if(.not.exit)then
         call wruser(' ',istat)
         call cnpar(name,cnstat)
         nbad=nbad+1
*
* GO BACK FOR A NEW ENTRY ONLY IF THE MAX. NUMBER OF
* BAD ENTRIES HAS NOT BEEN REACHED
*
         if(nbad.lt.maxbad)go to 11
      endif

      end
