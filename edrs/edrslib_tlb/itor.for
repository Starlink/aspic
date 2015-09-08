      subroutine itor
 
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO CONVERT AN INTEGER*2 IMAGE TO A REAL IMAGE
*
*METHOD
*       OBTAIN INPUT I*2 IMAGE AND OUTPUT REAL IMAGE OF THE SAME SIZE.
*       OBTAIN INPUT DESCRIPTOR ITEMS, THEN CALL IRCONV TO PERFORM THE
*       CONVERSION. UPDATE OUTPUT DESCRIPTOR.
*
*ARGUMENTS
*       NONE
*
*CALLS
*       THIS PACKAGE:
*               GT2DIR,GT2DIW,GTDSCR,PTDSCR
*       STARLINK:
*               CYDSCR,FRDATA
*
*NOTES
*       USES VAX %VAL FACILITY
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*----------------------------------------------------------------------
*
*
      character cval*1
 
*
* OBTAIN INPUT IMAGE
*
      call gt2dir('INPUT',102,.false.,npix,nlines,ipin,ierr)
 
      if(ierr.ne.0)go to 99
 
*
* OBTAIN OUTPUT IMAGE
*
      call gt2diw('OUTPUT',204,.false.,npix,nlines,ipout,ierr)
 
      if(ierr.ne.0)go to 99
 
*
* OBTAIN INPUT DESCRIPTOR ITEMS REQUIRED
*
      inval=-100000
      scale=1.0
      zero=0.0
      call gtdscr('INPUT','INVAL','INTEGER',inval,rval,cval,ierr)
      call gtdscr('INPUT','BSCALE','REAL',ival,scale,cval,ierr)
      call gtdscr('INPUT','BZERO','REAL',ival,zero,cval,ierr)
 
*
* PERFORM THE CONVERSION
*
      call irconv(%val(ipin),npix,nlines,inval,scale,zero,%val(ipout))
 
*
* UPDATE OUTPUT DESCRIPTOR
*
      call cydscr('INPUT','OUTPUT',istat)
      call ptdscr('OUTPUT','INVAL','INTEGER',0,rval,cval,ierr)
      call ptdscr('OUTPUT','BSCALE','REAL',ival,1.0,cval,ierr)
      call ptdscr('OUTPUT','BZERO','REAL',ival,0.0,cval,ierr)
 
*
* RELEASE DATA AREAS AND RETURN
*
99    call frdata(' ',istat)
 
      end
 
 
 
