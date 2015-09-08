      subroutine ptstds(fname,npix,nlines,nim,inval,bscale,bzero,title)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO UPDATE THE DESCRIPTORS SPECIFYING THE SIZE OF A 2 OR 3D
*       IMAGE FRAME AND TO INSERT THE STANDARD DESCRIPTOR ITEMS INVAL,
*       BSCALE, BZERO & TITLE.
*
*METHOD
*       CALL IMSZUD TO UPDATE THE SIZE ITEMS, THEN USE PTDSCN TO
*       INSERT THE OTHER STANDARD ITEMS.
*
*ARGUMENTS
*       FNAME (IN)
*       CHARACTER*(*)
*               THE PARAMETER NAME OF THE FRAME TO BE UPDATED
*       NPIX,NLINES (IN)
*       INTEGER
*               THE DIMENSIONS OF THE IMAGES IN THE FRAME
*       NIM (IN)
*       INTEGER
*               THE NUMBER OF IMAGE PLANES (1 FOR 2D IMAGES)
*       INVAL (IN)
*       INTEGER(NIM)
*               AN ARRAY OF INVALID PIXEL FLAGS TO BE INSERTED
*       BSCALE,BZERO (IN)
*       REAL(NIM)
*               ARRAYS CONTAINING THE IMAGE SCALE FACTORS AND ZEROS
*               TO BE INSERTED
*       TITLE (IN)
*       CHARACTER(NIM)*(*)
*               ARRAY OF TITLES TO BE INSERTED
*
*STARLINK PARAMETERS
*       THE FRAME GIVEN IN THE ARGUMENT 'FNAME' IS ACCESSED BY THIS
*       ROUTINE
*
*CALLS
*       THIS PACKAGE:
*               IMSZUD,PTDSCN
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
* UPDATE THE IMAGE SIZE PARAMETERS
*
      call imszud(fname,npix,nlines,nim)
 
*
* INSERT EACH STANDARD ITEM IN TURN
*
      call ptdscn(fname,'INVAL','INTEGER',1,nim,inval,rval,cval,ierr)
      call ptdscn(fname,'BSCALE','REAL',1,nim,ival,bscale,cval,ierr)
      call ptdscn(fname,'BZERO','REAL',1,nim,ival,bzero,cval,ierr)
      call ptdscn(fname,'TITLE','CHARACTER',1,nim,ival,rval,title,ierr)
 
      end
 
 
 
