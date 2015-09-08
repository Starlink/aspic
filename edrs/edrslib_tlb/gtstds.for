      subroutine gtstds(fname,nim,inval,bscale,bzero,title)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO OBTAIN THE STANDARD DESCRIPTOR ITEMS INVAL, BSCALE, BZERO & TITLE
*       FROM A 3D IMAGE FRAME, APPLYING THE APPROPRIATE DEFAULTS.
*
*METHOD
*       APPLY DEFAULTS, THEN CALL GTDSCN TO OBTAIN THE VALUES, FINALLY
*       CHECK THAT INVAL LIES IN RANGE
*
*ARGUMENTS
*       FNAME (IN)
*       CHARACTER*(*)
*               THE PARAMETER NAME OF THE FRAME TO BE READ
*       NIM (IN)
*       INTEGER
*               THE NUMBER OF IMAGE VALUES TO BE READ
*       INVAL (OUT)
*       INTEGER(NIM)
*               ARRAY TO HOLD THE INVALID PIXEL FLAGS
*       BSCALE, BZERO (OUT)
*       REAL(NIM)
*               ARRAYS TO HOLD THE IMAGE SCALE FACTORS AND ZEROS
*       TITLE (OUT)
*       CHARACTER(NIM)*(*)
*               ARRAY TO HOLD THE IMAGE TITLES
*
*STARLINK PARAMETERS
*       THE FRAME GIVEN IN THE ARGUMENT 'FNAME' IS ACCESSED BY THIS
*       ROUTINE
*
*CALLS
*       THIS PACKAGE:
*               GTDSCN
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      character fname*(*),title(nim)*(*),cval(1)*1
      integer inval(nim),ival(1)
      real bscale(nim),bzero(nim),rval(1)
 
*
* APPLY THE DEFAULTS TO EACH DESCRIPTOR ITEM IN TURN
*
 
      do 1 i=1,nim
         inval(i)=-100000
         bscale(i)=1.0
         bzero(i)=0.0
         title(i)=' '
1     continue
 
 
*
* NOW OBTAIN VALUES FROM INPUT FRAME
*
      call gtdscn(fname,'INVAL','INTEGER',1,nim,inval,rval,cval,ierr)
      call gtdscn(fname,'BSCALE','REAL',1,nim,ival,bscale,cval,ierr)
      call gtdscn(fname,'BZERO','REAL',1,nim,ival,bzero,cval,ierr)
      call gtdscn(fname,'TITLE','CHARACTER',1,nim,ival,rval,title,ierr)
 
*
* ALTER INVAL IF IT LIES OUTSIDE +/-32767
*
 
      do 2 i=1,nim
 
         if(abs(inval(i)).gt.32767) inval(i)=-32767
2     continue
 
 
      end
 
 
 
