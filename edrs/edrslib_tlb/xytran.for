      subroutine xytran
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO APPLY A LINEAR TRANSFORMATION TO AN X,Y LIST DATASET
*
*METHOD
*       OBTAIN INPUT DATASET. OBTAIN TRANSFORMATION COEFFICIENTS.
*       CALL XYLTRN TO APPLY THE TRANSFORMATION. UPDATE OUTPUT
*       DESCRIPTOR ITEMS.
*
*ARGUMENTS
*       NONE
*
*STARLINK PARAMETERS
*       INPUT
*               THE INPUT DATASET
*       OUTPUT
*               THE OUTPUT DATASET
*       TRCOEFFS
*               SIX REAL COEFFICIENTS DEFINING THE TRANSFORMATION
*       TITLE
*               A TITLE TO REPLACE THE INPUT TITLE IN THE OUTPUT DATASET
*
*CALLS
*       THIS PACKAGE:
*               GTXYLR,GTDSCR,GTXYLW,XYLTRN,PTDSCR
*       STARLINK:
*               RDKEYR,CYDSCR,RDKEYC,FRDATA
*
*NOTES
*       USES VAX %VAL FACILITY
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      character title(1)*30
      real c(6)
 
*
* OBTAIN INPUT LIST
*
      call gtxylr('INPUT',.false.,nitem,lstlen,ipin,ierrin)
 
      if(ierrin.eq.0) then
 
*
* INPUT LIST SUCCESSFULLY OBTAINED... EXTRACT TITLE FROM DESCRIPTOR
*
         title(1)=' '
         call gtdscr('INPUT','TITLE','CHARACTER',ival,rval,title(1)
     :    ,ierr)
 
*
* OBTAIN OUTPUT LIST DATA FRAME
*
         call gtxylw('OUTPUT',.false.,nitem,lstlen,ipout,ierrou)
 
         if(ierrou.eq.0) then
 
*
* OUTPUT FRAME SUCCESSFULLY OBTAINED... GET VALUES OF THE TRANSFORM
* COEFFICIENTS
*
            c(1)=0.0
            c(2)=1.0
            c(3)=0.0
            c(4)=0.0
            c(5)=0.0
            c(6)=1.0
            call rdkeyr('TRCOEFFS',.false.,6,c,nval,istat)
 
*
* CALL XYLTRN TO APPLY THE TRANSFORMATION TO THE INPUT LIST, PUTTING
* THE RESULT IN THE OUTPUT LIST
*
            call xyltrn(%val(ipin),nitem,lstlen,c,%val(ipout))
 
*
* COPY INPUT DESCRIPTOR TO OUTPUT AND UPDATE TITLE
*
            call cydscr('INPUT','OUTPUT',istat)
            call rdkeyc('TITLE',.true.,1,title,nval,istat)
            call ptdscr('OUTPUT','TITLE','CHARACTER',ival,rval,title(1)
     :       ,ierr)
         endif
 
      endif
 
 
*
* FREE DATA AREAS AND RETURN
*
      call frdata(' ',istat)
      return
 
      end
 
 
 
