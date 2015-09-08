      subroutine descri
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO EXTRACT DESCRIPTOR ITEMS FROM AN IMAGE AND ASSIGN THE VALUES
*       TO STARLINK PARAMETERS
*
*METHOD
*       OBTAIN IMAGE FRAME, EXTRACT DESCRIPTOR ITEMS. IF REQUIRED, PRINT
*       VALUES. ASSIGN VALUES TO STARLINK PARAMETERS
*
*ARGUMENTS
*       NONE
*
*STARLINK PARAMETERS
*       ILEVEL
*               INTERACTION LEVEL: CONTROLS PRINTING OF RESULTS
*       IMAGE
*               THE IMAGE
*       TITLE
*               OUTPUT PARAMETER: IMAGE TITLE
*       NAXIS1,NAXIS2
*               OUTPUT PARAMETERS: IMAGE DIMENSIONS
*       INVAL
*               OUTPUT PARAMETER: INVALID PIXEL FLAG FOR IMAGE
*       BSCALE,BZERO
*               OUTPUT PARAMETERS: IMAGE SCALE AND ZERO LEVEL
*
*CALLS
*       THIS PACKAGE:
*               GETPAR,GT2DIR,GTDSCR,LBGONE
*       STARLINK:
*               WRUSER,WRKEYC,WRKEYI,WRKEYR,FRDATA
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      integer npix(1),nlines(1),inval(1)
      character title(1)*30,cval*1,prbuf*80
      real scale(1),zero(1)
 
*
* GET INTERACTION LEVEL
*
      ilevel=2
      call getpar('ILEVEL','INTEGER',1,1.0,3.0,.true.,ilevel,rval,ierr)
 
*
* OBTAIN INPUT IMAGE
*
      call gt2dir('IMAGE',102,.false.,npix(1),nlines(1),ipin,ierr)
 
      if(ierr.eq.0) then
 
*
* IMAGE OBTAINED SUCCESSFULLY...EXTRACT DESCRIPTOR ITEMS
*
         title(1)=' '
         inval(1)=-100000
         scale(1)=1.0
         zero(1)=0.0
         call gtdscr('IMAGE','TITLE','CHARACTER',ival,rval,title(1)
     :    ,ierr)
         call gtdscr('IMAGE','INVAL','INTEGER',inval(1),rval,cval,ierr)
         call gtdscr('IMAGE','BSCALE','REAL',ival,scale(1),cval,ierr)
         call gtdscr('IMAGE','BZERO','REAL',ival,zero(1),cval,ierr)
 
*
* IF ILEVEL.GE.2 SHOW THE ITEMS TO THE USER
*
 
         if(ilevel.ge.2) then
            call wruser(' ',istat)
            write(prbuf,14)title(1)
14          format('   TITLE=',a)
            call wruser(prbuf,istat)
            call wruser(' ',istat)
            write(prbuf,15)npix(1)
15          format('   NUMBER OF PIXELS PER LINE=',i8)
            call lbgone(prbuf(31:))
            call wruser(prbuf,istat)
            call wruser(' ',istat)
            write(prbuf,16)nlines(1)
16          format('   NUMBER OF LINES=',i8)
            call lbgone(prbuf(21:))
            call wruser(prbuf,istat)
            call wruser(' ',istat)
            write(prbuf,17)inval(1)
17          format('   FLAG FOR INVALID PIXELS=',i8)
            call lbgone(prbuf(29:))
            call wruser(prbuf,istat)
            call wruser(' ',istat)
            write(prbuf,18)scale(1)
18          format('   IMAGE SCALE FACTOR=',ss,g13.6)
            call wruser(prbuf,istat)
            call wruser(' ',istat)
            write(prbuf,19)zero(1)
19          format('   IMAGE ZERO LEVEL=',ss,g13.6)
            call wruser(prbuf,istat)
            call wruser(' ',istat)
         endif
 
 
*
* WRITE THE RESULTS TO THE ENVIRONMENT
*
         call wrkeyc('TITLE',title,1,istat)
         call wrkeyi('NAXIS1',npix,1,istat)
         call wrkeyi('NAXIS2',nlines,1,istat)
         call wrkeyi('INVAL',inval,1,istat)
         call wrkeyr('BSCALE',scale,1,istat)
         call wrkeyr('BZERO',zero,1,istat)
      endif
 
 
*
* RELEASE IMAGE AND RETURN
*
      call frdata(' ',istat)
      return
 
      end
 
 
 
