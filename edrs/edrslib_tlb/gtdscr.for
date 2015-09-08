      subroutine gtdscr(fname,dname,type,ival,rval,cval,ierr)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO OBTAIN DESCRIPTOR ITEMS FROM DATA FRAMES AND CONVERT TO
*       NUMERICAL VALUES IF REQUIRED
*
*METHOD
*       READ DESCRIPTOR ITEM AS A CHARACTER STRING, CONVERT TO REAL OR
*       INTEGER AS REQUIRED
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
*       IVAL (OUT)
*       INTEGER
*               RETURNS INTEGER VALUE IF REQUIRED
*       RVAL (OUT)
*       REAL
*               RETURNS REAL VALUE IF REQUIRED
*       CVAL (OUT)
*       CHARACTER*(*)
*               RETURNS CHARACTER VALUE
*       IERR (OUT)
*       INTEGER
*               ERROR FLAG: ZERO FOR SUCCESS
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
      character dscpt(1)*30,fname*(*),dname*(*),type*(*),cval*(*)
 
*
* OBTAIN DESCRIPTOR ENTRY
*
      call rddscr(fname,dname,1,dscpt,nval,ierr)
 
*
* IF OBTAINED OK, CONTINUE
*
 
      if(ierr.eq.0) then
         cval=dscpt(1)
 
*
* IF TYPE IS 'REAL', CONVERT TO A REAL NUMBER
*
 
         if(type.eq.'REAL') then
            call ctor(dscpt(1),r,istat)
 
            if(istat.eq.0) then
               rval=r
 
            else
 
*
* RETURN ERROR CODE IF CONVERSION NOT OK
*
               ierr=istat
            endif
 
 
*
* IF TYPE IS 'INTEGER', CONVERT TO AN INTEGER
*
 
         else if(type.eq.'INTEGER') then
            call ctoi(dscpt(1),i,istat)
 
            if(istat.eq.0) then
               ival=i
 
            else
 
*
* RETURN ERROR CODE IF CONVERSION NOT OK
*
               ierr=istat
            endif
 
 
*
* IF NOT 'REAL' OR 'INTEGER', RETURN WITH JUST A CHARACTER VALUE
*
         endif
 
      endif
 
      return
 
      end
 
 
 
