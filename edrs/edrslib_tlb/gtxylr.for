      subroutine gtxylr(name,null,nitem,lstlen,ipoint,ierr)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO OBTAIN AN INPUT X,Y LIST FRAME AND HANDLE ANY
*       ERROR CONDITIONS WHICH MAY ARISE
*
*METHOD
*       OBTAIN INPUT FRAME, CHECK FOR ERROR CODES, GIVING ERROR MESSAGES
*       AS REQUIRED. CHECK DIMENSIONS ARE OK. IF ERROR CONDITIONS
*       ARISE, RETURN FOR A NEW FRAME OR ABORT AS APPROPRIATE
*
*ARGUMENTS
*       NAME (IN)
*       CHARACTER*(*)
*               NAME OF FRAME
*       NULL (IN)
*       LOGICAL
*               IF NULL IS .TRUE., A NULL ENTRY FOR A FRAME IS VALID
*               OTHERWISE IT IS AN ERROR CONDITION
*       NITEM (OUT)
*       INTEGER
*               NUMBER OF ITEMS IN LIST RECORDS
*       LSTLEN (OUT)
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
*               PARAMETER NAME FOR INPUT FRAME IS SPECIFIED IN THE
*               ARGUMENT 'NAME'
*       NONULL/ERROR/
*               ACCESSED IF A NULL IS ENTERED AND IS NOT VALID
*       NOTXYLST/ERROR/
*               ACCESSED IF A FRAME IS SPECIFIED WHICH IS NOT AN
*               X,Y LIST
*       NOACCESS/ERROR/
*               ACCESSED IF FRAME CANNOT BE ACCESSED
*       DIMINVAL/ERROR/
*               ACCESSED IF FRAME HAS INVALID DIMENSIONS
*
*CALLS
*       THIS PACKAGE:
*               GTDSCR
*       STARLINK:
*               RDDATA,WRERR,CNPAR,WRUSER
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      character name*(*),frtype*6,cval*1
      integer cnstat
      logical null,exit
      parameter (maxbad=3)
 
*
* INITIALLISE BAD ENTRY COUNTER AND CALL RDDATA TO OBTAIN DATA FRAME
*
      nbad=0
11    call rddata(name,-104,frtype,len,ipoint,istat)
      ierr=istat
 
*
* IF NULL ENTRY:
*
 
      if(istat.eq.1) then
 
*
* IF NULL IS NOT ACCEPTABLE GIVE MESSAGE AND RETURN FOR NEW ENTRY
*
 
         if(.not.null) then
            call wrerr('NONULL')
            call cnpar(name,cnstat)
 
*
* IF NULL IS OK, RETURN WITH IERR=1
*
 
         else
            exit=.true.
         endif
 
 
*
* IF FORMAT CONVERSION HAS BEEN TRIED, THIS IS NOT AN XYLIST FRAME
* RETURN FOR A NEW ONE
*
 
      else if((istat.eq.2).or.(istat.eq.8)) then
         call wrerr('NOTXYLST')
         exit=.false.
 
*
* IF NOT ACCESSIBLE, GIVE ERROR MESSAGE AND GET NEW FRAME
*
 
      else if(istat.eq.3) then
         call wrerr('NOACCESS')
         exit=.false.
 
*
* INVALID PARAMETER NAME: RETURN IERR=4
*
 
      else if(istat.eq.4) then
         exit=.true.
 
*
* INVALID SIZE: GIVE MESSAGE AND GET NEW FRAME
*
 
      else if(istat.eq.6) then
         call wrerr('DIMINVAL')
         exit=.false.
 
*
* IF NONE OF THESE... FRAME IS OK
* EXTRACT DIMENSIONS OF LIST TO CHECK VALIDITY
*
 
      else
         nitem=0
         lstlen=0
         call gtdscr(name,'NITEM','INTEGER',nitem,rval,cval,ierrn)
         call gtdscr(name,'LSTLEN','INTEGER',lstlen,rval,cval,ierrl)
 
*
* IF THE FRAME TYPE IS NOT 'XYLIST' OR THE DESCRIPTOR ITEMS
* DO NOT EXIST, OR HAVE INVALID VALUES, THIS IS NOT A VALID XYLIST...
* RETURN FOR NEW FRAME
*
 
         if((frtype.ne.'XYLIST').or.(nitem.lt.7).or.(lstlen.lt.1)) then
            call wrerr('NOTXYLST')
            ierr=7
            exit=.false.
 
         else
            exit=.true.
         endif
 
      endif
 
 
*
* IF A BAD ENTRY HAS BEEN MADE AND A NEW ONE IS TO BE OBTAINED,
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
 
 
 
