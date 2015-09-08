      subroutine gt2did(name,null,ierr)
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To set up an association between a Starlink parameter and a
*       2d image frame without mapping the image data.
*
*SOURCE
*       GT2DID.FOR in UTILITIES.TLB
*
*ARGUMENTS
*   INPUTS:
*       NAME    character The name of the Starlink parameter to associate with
*                       the image.
*       NULL    logical If true, a null entry for the frame is ok, otherwise
*                       it is an error condition
*   OUTPUTS:
*       IERR    integer Status return. 0 - Success.
*                                      1 - Success, but null entered.
*                                      4 - Bad parameter name given.
*                                      10- Too many bad values given.
*				       11- Open file quota exceeded
*SUBROUTINES CALLED
*	THIS PACKAGE (UTILITIES.TLB):
*		gtjpil,wrerr,wruser
*       INTERIM:
*               cnpar
*       EDRS:
*               gtdscr
*
*STARLINK PARAMETERS
*       'NAME'/read/    The parameter name for the input image is given in the
*                       argument 'name'.
*	FILQUOTA/error/ Accessed if the process has insufficient 
*			remaining open file quota
*       NONULL/error/   Accessed if a null entry is given and 'null' is false.
*       NOACCESS/error/ Accessed if the frame cannot be accessed.
*       NOT2DIM/error/  Accessed if the image is not 2 dimensional.
*       BADNAME/error/  Accessed if a bad parameter name is given.
*       INVNAXIS/error/ Accessed if FITS descriptor NAXIS has invalid value
*       NOTIMAGE/error/ Accessed if frame is not a FITS image
*       TOOBAD/error/   Accessed if too many bad value are given by the user.
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 25/8/87
*-----------------------------------------------------------------------
*
*
      character name*(*),descr*80,cval*1
      logical null,exit
      integer dims(2),cnstat,gtjpil

*
* SET MAXIMUM NUMBER OF BAD ENTRIES TO BE TOLERATED
*
      parameter (maxbad=3)

*
* CHECK THAT THE PROCESS HAS SUFFICIENT RESOURCES TO OPEN TWO FILE
* IF NOT GIVE MESSAGE AND RETURN WITH IERR=11
*
      nfiles=gtjpil('FILCNT',1,ierr)
      if(ierr.ne.0) goto 999
      if(nfiles.lt.2) then
         call wrerr('FILQUOTA')
         ierr=11
         goto 999
      endif

*
* INITIALLISE BAD ENTRY COUNTER AND OBTAIN REQUIRED IMAGE FRAME
*
      nbad=0
11    call gtdscr(name,'NAXIS','INTEGER',ndim,rval,cval,ierr)

*
* IF NULL ENTERED:
*
      if(ierr.eq.1) then

*
* IF NULL IS NOT ALLOWED GIVE MESSAGE AND RETURN FOR NEW ENTRY
*
         if(.not.null) then
            call wrerr('NONULL')
            exit=.false.
         else

*
* IF NULL ENTRY IS OK, RETURN WITH IERR=1
*
            exit=.true.
         endif

*
* IF DESCRIPTOR 'NAXIS' WAS NOT FOUND, FRAME IS NOT AN IMAGE. GIVE MESSAGE AND
* RETURN FOR NEW FRAME
*
      else if(ierr.eq.2) then
         call wrerr('NOTIMAGE')
         exit=.false.

*
* IF FRAME COULD NOT BE ACCESSED, GIVE MESSAGE AND RETURN FOR NEW FRAME
*
      else if(ierr.eq.3) then
         call wrerr('NOACCESS')
         exit=.false.

*
* IF PARAMETER NAME IS INVALID, GIVE MESSAGE AND RETURN WITH IERR=4
*
      else if(ierr.eq.4) then
         exit=.true.
         call wrerr('BADNAME')

*
* IF NAXIS HAS INVALID VALUE, GIVE MESSAGE AND RETURN FOR NEW FRAME
*
      else if(ierr.eq.5) then
         exit=.false.
         call wrerr('INVNAXIS')

*
* IF NONE OF THE ABOVE: FRAME IS A GOOD IMAGE
*
      else

*
* IF NOT 2 DIMENSIONAL GIVE MESSAGE AND GET NEW FRAME
*
         if(ndim.ne.1.and.ndim.ne.2) then
            call wrerr('NOT2DIM')
            exit=.false.
         else

*
* OTHERWISE FRAME IS GOOD
*
            exit=.true.
         endif
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
* BAD ENTRIES HAS NOT BEEN REACHED. OTHERWISE SEND MESSAGE AND
* RETURN WITH IERR=10
*
         if(nbad.lt.maxbad)go to 11
         ierr=10
         call wrerr('TOOBAD')
      endif

*
* FINISH
*
  999 continue

      end
