      subroutine ptdscn(fname,dname,type,n1,n2,ival,rval,cval,ierr)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO ADD INTEGER,REAL AND CHARACTER VALUES TO THE DESCRIPTOR
*       ASSOCIATED WITH PARTICULAR IMAGES IN A 3D IMAGE FRAME
*
*METHOD
*       CONVERT TO A CHARACTER STRING ACCORDING TO TYPE. ADD TO
*       DESCRIPTOR IN THE NECESSARY POSITIONS FOR THE IMAGES CONCERNED
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
*       N1,N2 (IN)
*       INTEGER
*               INTEGERS SPECIFYING THE RANGE OF IMAGES WHOSE DESCRIPTORS
*               ARE TO BE CHANGED
*       IVAL (IN)
*       INTEGER(N1:N2)
*               THE INTEGER ITEMS, IF USED
*       RVAL (IN)
*       REAL(N1:N2)
*               THE REAL ITEMS, IF USED
*       CVAL (IN)
*       CHARACTER(N1:N2)*(*)
*               THE CHARACTER ITEMS, IF USED
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
*               RTOC,ITOC,RDDSCR,WRDSCR
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
* DEFINE MAXIMUM NUMBER OF IMAGES TO BE HANDLED
*
      parameter (mxdscr=100)
 
*
* DIMENSION ARRAYS
*
      character dscpt(mxdscr)*80,fname*(*),dname*(*),type*(*),
     :cval(n1:n2)*(*)
      integer ival(n1:n2)
      real rval(n1:n2)
      ierr=0
 
*
* OBTAIN EXISTING VALUES FOR THIS DESCRIPTOR (IF PRESENT)
*
      call rddscr(fname,dname,mxdscr,dscpt,nval,ipres)
 
      if(ipres.ne.0)nval=0
 
*
* IF THERE ARE FEWER THAN N1 VALUES THERE AT PRESENT, INSERT BLANK VALUES
* TO FILL SPACE
*
 
      do 1 n=nval+1,n1-1
         dscpt(n)=' '
1     continue
 
 
*
* NOW CONSIDER EACH NEW VALUE IN TURN
*
 
      do 2 n=max(1,n1),min(n2,mxdscr)
 
*
* CONVERT INPUT VALUE TO A CHARACTER STRING, ACCORDING TO THE TYPE
* AND PUT IN APPROPRIATE ARRAY ELEMENT
*
 
         if(type.eq.'CHARACTER') then
            dscpt(n)=cval(n)
 
         else if(type.eq.'REAL') then
            call rtoc(rval(n),dscpt(n),ierr)
 
         else if(type.eq.'INTEGER') then
            call itoc(ival(n),dscpt(n),ierr)
         endif
 
2     continue
 
 
*
* IF SUCCESSFUL, ADD CHARACTERS TO DESCRIPTOR
*
 
      if(ierr.eq.0) then
         call wrdscr(fname,dname,dscpt,max(nval,n2),ierr)
      endif
 
      return
 
      end
 
 
 
