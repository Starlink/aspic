      subroutine gt3dir(name,ifmt,null,npix,nlines,nim,ipoint,ierr)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO OBTAIN AN INPUT 3 DIMENSIONAL IMAGE FRAME AND HANDLE ANY
*       ERRORS WHICH OCCUR
*
*METHOD
*       OBTAIN INPUT IMAGE FRAME, CHECK FOR SUCCESS. IF AN ERROR OCCURS
*       EXIT OR RETURN TO GET A NEW FRAME AS APPROPRIATE. CALCULATE
*       POINTERS TO EACH IMAGE PLANE IN THE FRAME.
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
*       NIM (OUT)
*       INTEGER
*               THE NUMBER OF IMAGES
*       IPOINT (OUT)
*       INTEGER(*)
*               POINTERS TO THE IMAGE PLANES IN THE FRAME
*               LENGTH OF ARRAY MUST BE AT LEAST NIM
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
*       NOT3DIM/ERROR/
*               ACCESSED IF THE IMAGE HAS MORE THAN 3 DIMENSIONS
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
      integer dims(3),cnstat,ipoint(*)
 
*
* SET MAXIMUM NUMBER OF BAD ENTRIES TO BE TOLERATED
*
      parameter (maxbad=3)
 
*
* INITIALLISE BAD ENTRY COUNTER AND OBTAIN REQUIRED IMAGE FRAME
*
      nbad=0
11    call rdimag(name,ifmt,3,dims,ndims,ipoint(1),istat)
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
* IF NONE OF THE ABOVE, FRAME IS GOOD
* CHECK THE NUMBER OF DIMENSIONS IS 1,2 OR 3
*
 
      else
 
         if(ndims.eq.1) then
            npix=dims(1)
            nlines=1
            nim=1
            exit=.true.
 
         else if(ndims.eq.2) then
            npix=dims(1)
            nlines=dims(2)
            nim=1
            exit=.true.
 
         else if(ndims.eq.3) then
            npix=dims(1)
            nlines=dims(2)
            nim=dims(3)
            exit=.true.
 
         else
            call wrerr('NOT3DIM')
            ierr=9
            exit=.false.
         endif
 
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
 
 
*
* IF AN IMAGE WAS OBTAINED, CHECK THE NUMBER OF DIMENSIONS
* AND SET NPIX, NLINES & NIM APPROPRIATELY
*
*
* CALCULATE POINTERS TO EACH IMAGE PLANE
*
 
      if(ierr.eq.0) then
         nbyte=npix*nlines*mod(ifmt,100)
 
         do 7 i=2,nim
            ipoint(i)=ipoint(i-1)+nbyte
7        continue
 
      endif
 
      return
 
      end
 
 
 
