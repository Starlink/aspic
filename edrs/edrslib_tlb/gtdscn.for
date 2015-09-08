      subroutine gtdscn(fname,dname,type,n1,n2,ival,rval,cval,ierr)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO OBTAIN DESCRIPTOR ITEMS CORRESPONDING TO SPECIFIC IMAGES
*       IN A 3D IMAGE FRAME
*
*METHOD
*       READ DESCRIPTOR ITEM AS AN ARRAY OF CHARACTER STRINGS, THEN
*       CONVERT THOSE CORRESPONDING TO THE IMAGES REQUIRED TO REAL OR
*       INTEGER IF NECESSARY.
*
*ARGUMENTS
*       FNAME (IN)
*       CHARACTER*(*)
*               NAME OF FRAME TO BE USED
*       DNAME (IN)
*       CHARACTER*(*)
*               NAME OF DESCRIPTOR ITEM
*       TYPE (IN)
*       CHARACTER*(*)
*               'REAL', 'INTEGER' OR 'CHARACTER': GIVES TYPE REQUIRED
*       N1,N2 (IN)
*       INTEGER
*               INTEGERS SPECIFYING THE RANGE OF IMAGE NUMBERS FOR
*               WHICH DESCRIPTORS ARE REQUIRED
*       IVAL (OUT)
*       INTEGER(N1:N2)
*               RETURNS INTEGER VALUES IF REQUIRED
*               IF THERE IS NO VALUE IN THE FRAME, THE CORRESPONDING
*               ELEMENT OF THE OUTPUT ARRAY IS UNCHANGED ON EXIT
*       RVAL (OUT)
*       REAL(N1:N2)
*               RETURNS REAL VALUES IF REQUIRED
*               (DEFAULT AS FOR IVAL)
*       CVAL (OUT)
*       CHARACTER(N1:N2)*(*)
*               RETURNS CHARACTER VALUES
*       IERR (OUT)
*       INTEGER
*               ERROR FLAG: ZERO FOR SUCCESS
*                           NONZERO IF DESCRIPTOR NOT PRESENT IN FRAME
*
*STARLINK PARAMETERS
*       'FNAME'
*               THE PARAMETER NAME OF THE FRAME TO BE ACCESSED IS GIVEN
*               IN THE ARGUMENT 'FNAME'
*
*CALLS
*       STARLINK:
*               RDDSCR,CTOR,CTOI
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
* DEFINE MAX NUMBER OF IMAGES TO BE HANDLED
*
      parameter (mxdscr=100)
 
*
* DIMENSION ARRAYS
*
      character dscpt(mxdscr)*80,fname*(*),dname*(*),type*(*),
     :cval(n1:n2)*(*)
      real rval(n1:n2)
      integer ival(n1:n2)
 
*
* OBTAIN DESCRIPTOR ENTRY
*
      call rddscr(fname,dname,max(n2,mxdscr),dscpt,nval,ierr)
 
*
* IF OBTAINED OK, CONTINUE
*
 
      if(ierr.eq.0) then
 
*
* SCAN THROUGH REQUIRED IMAGES
*
 
         do 1 n=max(1,n1), min(nval,n2)
 
*
* IF TYPE IS 'REAL', CONVERT TO A REAL NUMBER
*
 
            if(type.eq.'REAL') then
               call ctor(dscpt(n),r,istat)
 
               if(istat.eq.0) rval(n)=r
 
*
* IF TYPE IS 'INTEGER', CONVERT TO AN INTEGER
*
 
            else if(type.eq.'INTEGER') then
               call ctoi(dscpt(n),i,istat)
 
               if(istat.eq.0) ival(n)=i
 
*
* IF TYPE IS 'CHARACTER', RETURN WITH CHARACTER VALUE
*
 
            else if(type.eq.'CHARACTER') then
 
               if(dscpt(n).ne.' ')cval(n)=dscpt(n)
            endif
 
1        continue
 
      endif
 
      return
 
      end
 
 
 
