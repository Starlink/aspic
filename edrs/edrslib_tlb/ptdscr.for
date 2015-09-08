      subroutine ptdscr(fname,dname,type,ival,rval,cval,ierr)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO ADD INTEGER,REAL AND CHARACTER VALUES TO A DESCRIPTOR
*
*METHOD
*       CONVERT TO A CHARACTER STRING ACCORDING TO TYPE. ADD TO
*       DESCRIPTOR
*
*ARGUMENTS
*       FNAME (IN)
*       CHARACTER*(*)
*               THE PARAMETER NAME OF THE FRAME TO WHICH THE VALUE IS
*               ADDED
*       DNAME (IN)
*       CHARACTER*(*)
*               THE DESCRIPTOR ITEM NAME
*       TYPE (IN)
*       CHARACTER*(*)
*               ONE OF: 'REAL', 'INTEGER' OR 'CHARACTER'.. SPECIFIES
*               THE ITEM TYPE
*       IVAL (IN)
*       INTEGER
*               THE INTEGER ITEM, IF USED
*       RVAL (IN)
*       REAL
*               THE REAL ITEM, IF USED
*       CVAL (IN)
*       CHARACTER*(*)
*               THE CHARACTER ITEM, IF USED
*       IERR (OUT)
*       INTEGER
*               ERROR FLAG: ZERO FOR SUCCESS
*
*STARLINK PARAMETERS
*       THE FRAME SPECIFIED BY THE PARAMETER GIVEN IN 'FNAME' IS
*       ACCESSED BY THIS ROUTINE
*
*CALLS
*       STARLINK:
*               RTOC,ITOC,WRDSCR
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      character dscpt(1)*30,fname*(*),dname*(*),type*(*),cval*(*)
      ierr=0
 
*
* CONVERT INPUT VALUE TO A CHARACTER STRING, ACCORDING TO THE TYPE
*
 
      if(type.eq.'CHARACTER') then
         dscpt(1)=cval
 
      else if(type.eq.'REAL') then
         call rtoc(rval,dscpt(1),ierr)
 
      else if(type.eq.'INTEGER') then
         call itoc(ival,dscpt(1),ierr)
      endif
 
 
*
* IF SUCCESSFUL, ADD CHARACTERS TO DESCRIPTOR
*
 
      if(ierr.eq.0) then
         call wrdscr(fname,dname,dscpt,1,ierr)
      endif
 
      return
 
      end
 
 
 
