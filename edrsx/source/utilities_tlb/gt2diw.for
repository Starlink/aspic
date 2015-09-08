      subroutine gt2diw(name,ifmt,null,npix,nlines,ipoint,ierr)

*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO OBTAIN AN OUTPUT 2 DIMENSIONAL IMAGE FRAME AND HANDLE ANY
*       ERRORS WHICH OCCUR
*
*METHOD
*       CHECK THAT THE CALLING PROCESS CAN OPEN ANOTHER FILE. IF IT CAN
*       OBTAIN THE OUTPUT FRAME OF THE REQUIRED SIZE. IF ERRORS OCCUR
*       EXIT OR RETURN FOR A NEW FRAME AS APPROPRIATE
*
*ARGUMENTS
*       NAME (IN)
*       CHARACTER*(*)
*               THE PARAMETER NAME FOR THE OUTPUT FRAME
*       IFMT (IN)
*       INTEGER
*               THE STARLINK FORMAT CODE FOR THE OUTPUT FRAME
*       NULL (IN)
*       LOGICAL
*               IF TRUE, A NULL VALUE FOR THE FRAME IS OK. OTHERWISE
*               IT IS AN ERROR CONDITION
*       NPIX,NLINES (IN)
*       INTEGER
*               THE OUTPUT IMAGE DIMENSIONS
*       IPOINT (OUT)
*       INTEGER
*               THE POINTER TO THE DATA FRAME
*       IERR (OUT)
*       INTEGER
*               ERROR FLAG: ZERO FOR SUCCESS
*
*STARLINK PARAMETERS
*       'NAME'
*               THE PARAMETER NAME FOR THE OUTPUT FRAME IS GIVEN IN
*               THE ARGUMENT 'NAME'
*       NONULL/ERROR/
*               ACCESSED IF A NULL VALUE IS GIVEN FOR THE FRAME AND
*               'NULL' IS FALSE
*       NOACCESS/ERROR/
*               ACCESSED IF THE FRAME CANNOT BE ACCESSED
*       DIMOOR/ERROR/
*               ACCESSED IF THE IMAGE DIMENSIONS ARE OUT OF RANGE
*       FILQUOTA/ERROR/
*               ACCESSED IF THE CALLING PROCESS CANT OPEN ANOTHER FILE
*
*CALLS
*       STARLINK:
*               WRIMAG,WRERR,CNPAR,WRUSER
*       IRAS UTILITIES:
*               GTJPIL
*
*WRITTEN BY
*       R.F. WARREN-SMITH, modified by DS Berry to check open file quota
*-----------------------------------------------------------------------
*
*
      character name*(*)
      logical null,exit
      integer dims(2),cnstat,gtjpil

*
* SET MAX NUMBER OF BAD ENTRIES PERMITTED
*
      parameter (maxbad=3)

*
* CHECK THAT THE PROCESS HAS SUFFICIENT RESOURCES TO OPEN TWO FILES
* IF NOT GIVE MESSAGE AND RETURN WITH IERR=10
*
      nfiles=gtjpil('FILCNT',1,ierr)
      if(ierr.ne.0) goto 999
      if(nfiles.lt.2) then
         call wrerr('FILQUOTA')
         ierr=10
         goto 999
      endif
*
* SET DIMENSIONS OF IMAGE
*
      dims(1)=npix
      dims(2)=nlines

*
* INITIALLISE BAD ENTRY COUNTER AND CALL WRIMAG TO OBTAIN IMAGE FRAME
*
      nbad=0
11    call wrimag(name,ifmt,dims,2,ipoint,istat)
      ierr=istat

*
* IF NULL WAS ENTERED:
*

      if(istat.eq.1) then

*
* IF NULL IS NOT PERMITTED GIVE MESSAGE AND GO BACK FOR A NEW ENTRY
*

         if(.not.null) then
            call wrerr('NONULL')
            exit=.false.

*
* IF NULL IS OK, RETURN WITH IERR=1
*

         else
            exit=.true.
         endif


*
* IF FRAME CANNOT BE ACCESSED, GIVE MESSAGE AND RETURN FOR NEW FRAME
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
* IF FORMAT CODE IS INVALID, RETURN WITH IERR=5
*

      else if(istat.eq.5) then
         exit=.true.

*
* IF IMAGE DIMENSIONS ARE INVALID, GIVE MESSAGE AND RETURN WITH
* IERR=6
*

      else if(istat.eq.6) then
         call wrerr('DIMOOR')
         exit=.true.

*
* IF NONE OF THE ABOVE: IMAGE IS OK
*

      else
         exit=.true.
      endif


*
* IF A BAD ENTRY WAS MADE AND A NEW ONE IS TO BE OBTAINED,
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

 999  continue

      end



