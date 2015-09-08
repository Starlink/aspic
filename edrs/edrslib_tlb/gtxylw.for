      subroutine gtxylw(name,null,nitem,lstlen,ipoint,ierr)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO OBTAIN AN OUTPUT X,Y LIST FRAME AND HANDLE ANY ERROR
*       CONDITIONS WHICH MAY ARISE
*
*METHOD
*       OBTAIN OUTPUT FRAME OF REQUIRED SIZE. CHECK FOR ERROR CODES.
*       GIVE ERROR MESSAGES AND RETURN FOR A NEW FRAME IF ERRORS
*       ARISE, OR ABORT IF APPROPRIATE.
*
*ARGUMENTS
*       NAME (IN)
*       CHARACTER*(*)
*               NAME OF FRAME
*       NULL (IN)
*       LOGICAL
*               IF NULL IS .TRUE., A NULL FRAME ENTRY IS VALID,
*               OTHERWISE IT IS AN ERROR CONDITION
*       NITEM (IN)
*       INTEGER
*               NUMBER OF ITEMS IN A LIST RECORD
*       LSTLEN (IN)
*       INTEGER
*               NUMBER OF LIST ENTRIES (RECORDS)
*       IPOINT (OUT)
*       INTEGER
*               POINTER TO DATA FRAME
*       IERR (OUT)
*       INTEGER
*               ERROR FLAG: ZERO FOR SUCCESS
*
*STARLINK PARAMETERS
*       'NAME'
*               PARAMETER NAME FOR THE OUTPUT FRAME IS SPECIFIED
*               IN THE ARGUMENT 'NAME'
*       NONULL/ERROR/
*               ACCESSED IF NULL IS GIVEN AND IT IS NOT VALID
*       NOACCESS/ERROR/
*               ACCESSED IF FRAME GIVEN CANNOT BE ACCESSED
*       DIMOOR/ERROR/
*               ACCESSED IF FRAME HAS INVALID DIMENSIONS
*
*CALLS
*       STARLINK:
*               WRDATA,WRERR,CNPAR,WRUSER
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      character name*(*)
      integer cnstat
      logical null,exit
      parameter (maxbad=3)
 
*
* INITIALLISE BAD ENTRY COUNTER AND CALL WRDATA TO OBTAIN DATA FRAME
*
      nbad=0
      len=nitem*lstlen
11    call wrdata(name,104,'XYLIST',len,ipoint,istat)
      ierr=istat
 
*
* IF NULL ENTRY..
*
 
      if(istat.eq.1) then
 
*
* IF NULL IS NOT ACCEPTABLE GIVE MESSAGE AND RETURN FOR NEW FRAME
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
* IF NOT ACCESSIBLE, GIVE MESSAGE AND GET NEW FRAME
*
 
      else if(istat.eq.3) then
         call wrerr('NOACCESS')
         exit=.false.
 
*
* INVALID PARAMETER NAME... RETURN WITH IERR=4
*
 
      else if(istat.eq.4) then
         exit=.true.
 
*
* INVALID FRAME SIZE... RETURN WITH IERR=6
*
 
      else if(istat.eq.6) then
         call wrerr('DIMOOR')
         exit=.true.
 
*
* IF NONE OF THE ABOVE.. FRAME IS OK
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
 
 
 
