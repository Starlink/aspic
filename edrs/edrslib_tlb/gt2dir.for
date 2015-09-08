      subroutine gt2dir(name,ifmt,null,npix,nlines,ipoint,ierr)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO OBTAIN AN INPUT 2 DIMENSIONAL IMAGE FRAME AND HANDLE ANY
*       ERRORS WHICH OCCUR
*
*METHOD
*       OBTAIN INPUT IMAGE FRAME, CHECK FOR SUCCESS. IF AN ERROR OCCURS
*       EXIT OR RETURN TO GET A NEW FRAME AS APPROPRIATE.
*
*ARGUMENTS
*       NAME (IN)
*       CHARACTER*(*)
*               THE PARAMETER NAME USED TO ACCESS THE INPUT FRAME
*       IFMT (IN)
*       INTEGER
*               THE STARLINK FORMAT CODE FOR THE FRAME
*       NULL (IN)
*       LOGICAL
*               IF TRUE, A NULL ENTRY FOR THE FRAME IS OK, OTHERWISE
*               IT IS AN ERROR CONDITION
*       NPIX,NLINES (OUT)
*       INTEGER
*               THE IMAGE DIMENSIONS
*       IPOINT (OUT)
*       INTEGER
*               THE POINTER TO THE DATA FRAME
*       IERR (OUT)
*       INTEGER
*               ERROR FLAG: ZERO FOR SUCCESS
*
*STARLINK PARAMETERS
*       'NAME'
*               THE PARAMETER NAME FOR THE INPUT IMAGE IS GIVEN IN THE
*               ARGUMENT 'NAME'
*       NONULL/ERROR/
*               ACCESSED IF A NULL ENTRY IS GIVEN AND 'NULL' IS FALSE
*       NOACCESS/ERROR/
*               ACCESSED IF THE FRAME CANNOT BE ACCESSED
*       DIMINVAL/ERROR/
*               ACCESSED IF THE IMAGE DIMENSIONS ARE INVALID
*       NOTIMAGE/ERROR/
*               ACCESSED IF THE FRAME DOES NOT CONTAIN AN IMAGE
*       FMTFAIL/ERROR/
*               ACCESSED IF FORMAT CONVERSION FAILS
*       NOT2DIM/ERROR/
*               ACCESSED IF THE IMAGE IS NOT 2 DIMENSIONAL
*
*CALLS
*       STARLINK:
*               RDIMAG,WRERR,CNPAR,WRUSER
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      character name*(*)
      logical null,exit
      integer dims(2),cnstat
 
*
* SET MAXIMUM NUMBER OF BAD ENTRIES TO BE TOLERATED
*
      parameter (maxbad=3)
 
*
* INITIALLISE BAD ENTRY COUNTER AND OBTAIN REQUIRED IMAGE FRAME
*
      nbad=0
11    call rdimag(name,ifmt,2,dims,ndims,ipoint,istat)
      ierr=istat
 
*
* IF NULL ENTERED:
*
 
      if(istat.eq.1) then
 
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
* IF FORMAT CODE IS INVALID, RETURN IERR=5
*
 
      else if(istat.eq.5) then
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
* IF FORMAT CONVERSION FAILED, GIVE MESSAGE AND RETURN FOR NEW FRAME
*
 
      else if(istat.eq.8) then
         call wrerr('FMTFAIL')
         exit=.false.
 
*
* IF NONE OF THE ABOVE: FRAME IS A GOOD IMAGE
* CHECK THE NUMBER OF DIMENSIONS IS 1 OR 2
*
 
      else
 
         if(ndims.eq.1)then
            npix=dims(1)
            nlines=1
            exit=.true.
 
         else if(ndims.eq.2) then
            npix=dims(1)
            nlines=dims(2)
            exit=.true.
 
         else
 
*
* IF NOT 2 DIMENSIONAL GIVE MESSAGE AND GET NEW FRAME
*
            call wrerr('NOT2DIM')
            ierr=9
            exit=.false.
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
* BAD ENTRIES HAS NOT BEEN REACHED
*
 
         if(nbad.lt.maxbad)go to 11
      endif
 
 
      end
 
 
 
