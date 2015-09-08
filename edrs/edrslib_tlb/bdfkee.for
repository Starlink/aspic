      subroutine bdfkee
 
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
* PURPOSE
*          TO WRITE AN IMAGE IN AT THE TERMINAL AND OBTAIN AN OUTPUT
*         BDF FILE WRITTEN IN EDRS FORMAT.
* METHOD
*          THE IMAGE NAME,TITLE,NPIX AND NLINES ARE OBTAINED
*         FROM THE ENVIROMENT.IMAGE VALUES ARE ASKED FOR FROM THE USER.
*         ALL INVALID PIXELS MUST BE INPUT AS 999.THE DEFAULT IS ALSO
*         999.THE STARLINK IMAGE IS OBTAINED THRU A CALL TO GT2DIW
*         AND PASSED TO A LOWER LEVEL (TRANSTOBDF).FINALLY ALL NECCESARY
*         DESCRIPTORS ARE WRITTEN INTO THE IMAGE BDF FILE.
* STARLINK PARAMETERS
*
*         'OUTPUT' - PARAMETER NAME OF OUTPUT BDF FILE
*         'NLINES' - NO. OF LINES IN OP FILE
*         'NPIX'   - NO. OF PIXELS IN OP FILE
*
* CALLS
*         EDRS:GETPAR,PTDSCR,TRANTBDF
*         STARLINK:GETPAR,RDKEYC,RDKEYR,GT2DIW,CNPAR,FRDATA,WRUSER,WRERR
* NOTES
*         USES %VAL VACILITY
* WRITTEN BY
*         J.V.SHIRT
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*
      character title*30
      logical vmgot,vmfree
 
*
* OBTAIN NO LINES AND PIXELS IN IMAGE
      call wruser('GIVE NO OF PIXELS AND OF LINES IN OUTPUT IMAGE',
     :ierr)
      nlines=1
      npix=1
      call getpar('NLINES','INTEGER',1,1.,10000.,.true.,nlines,rval
     : ,istat)
      call getpar('NPIX','INTEGER',1,1.,10000.,.true.,npix,rval,istat)
 
*
* OBTAIN NEEDED WORKSPACE FOR CONVERTION FROM LINE TO BDF FILE
*
      laqu=4*npix
      ldim=4*nlines*npix
      vmgot=.true.
      vmgot=vmgot.and.lib$get_vm(ldim,idim)
      vmgot=vmgot.and.lib$get_vm(laqu,iaqu)
 
      if(.not.vmgot)then
         call wrerr('NOSPACE')
         goto 999
 
      endif
 
 
*
* IF SUCCESSFUL OBTAIN OUTPUT IMAGE
*
      call gt2diw('OUTPUT',102,.false.,npix,nlines,ipoint,ierr2)
 
*
*  ZERO IMPLIES SUCCESS!
*
 
      if (ierr2.eq.0)then
 
* OBTAIN TITLE OF OUTPUT FRAME
*
         title=' '
         call rdkeyc('TITLE',.false.,1,title,nval,istat)
 
*
         call trantbdf(%val(ipoint),npix,nlines,%val(iaqu),%val(idim)
     :    ,bzero,bscale)
 
*
         call ptdscr('OUTPUT','NAXIS','INTEGER',2,rval,cval,ierr)
         call ptdscr('OUTPUT','NAXIS1','INTEGER',npix,rval,cval,ierr)
         call ptdscr('OUTPUT','NAXIS2','INTEGER',nlines,rval,cval,ierr)
         call ptdscr('OUTPUT','BZERO','REAL',ival,bzero,cval,ierr)
         call ptdscr('OUTPUT','BSCALE','REAL',ival,bscale,cval,ierr)
         call ptdscr('OUTPUT','TITLE','CHARACTER',ival,rval,title,ierr)
         call ptdscr('OUTPUT','INVAL','INTEGER',-32767,rval,cval,ierr)
      endif
 
 
*
* FREE DATA AREAS
*
      call frdata(' ',istat)
 
*
* FREE WORKSPACE
*
      vmfree=.true.
      vmfree=vmfree.and.lib$free_vm(ldim,idim)
      vmfree=vmfree.and.lib$free_vm(laqu,iaqu)
 
999   end
 
 
 
