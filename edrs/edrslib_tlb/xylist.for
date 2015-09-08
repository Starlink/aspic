      subroutine xylist
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO PRINT THE CONTENTS OF AN X,Y LIST DATASET
*
*METHOD
*       OBTAIN INPUT DATASET. PRINT TITLE IF PRESENT. CALL PRLIST TO
*       PRINT THE DATA
*
*ARGUMENTS
*       NONE
*
*STARLINK PARAMETERS
*       INPUT
*               THE INPUT X,Y LIST DATASET
*
*CALLS
*       THIS PACKAGE:
*               GTXYLR,GTDSCR,PRLIST
*       STARLINK:
*               WRUSER,FRDATA
*
*NOTES
*       USES VAX %VAL FACILITY
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      character cval*1,title*30
 
*
* OBTAIN INPUT XY LIST
*
      call gtxylr('INPUT',.false.,nitem,lstlen,ipin,ierrxy)
 
      if(ierrxy.eq.0) then
 
*
* INPUT SUCCESSFULLY OBTAINED... EXTRACT TITLE
*
         title=' '
         call gtdscr('INPUT','TITLE','CHARACTER',ival,rval,title,ierr)
 
*
* IF TITLE PRESENT, PRINT IT
*
         call wruser(' ',istat)
         call wruser(' ',istat)
 
         if(title.ne.' ') then
            call wruser('                      TITLE: '//title,istat)
            call wruser(' ',istat)
         endif
 
 
*
* CALL PRLIST TO PRINT THE CONTENTS OF THE LIST
*
         call prlist(%val(ipin),nitem,lstlen)
      endif
 
 
*
* FREE DATA AREA AND RETURN
*
      call frdata(' ',istat)
      return
 
      end
 
 
 
