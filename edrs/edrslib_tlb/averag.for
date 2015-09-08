      subroutine averag
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO FIND THE AVERAGE VALUE OF ALL VALID PIXELS IN AN IMAGE
*
*METHOD
*       OBTAIN THE IMAGE AND DESCRIPTOR ITEMS, SUM THE VALID PIXELS
*       AND FORM THE MEAN. WRITE RESULT TO ENVIRONMENT.
*
*ARGUMENTS
*       NONE
*
*STARLINK PARAMETERS
*       ILEVEL
*               INTERACTION LEVEL: PRINTS RESULTS IF ILEVEL>1
*       INPUT
*               INPUT IMAGE FRAME
*       AVERAGE
*               OUTPUT PARAMETER: AVERAGE VALUE
*       NGOOD
*               OUTPUT PARAMETER: NUMBER OF VALID PIXELS (INTEGER)
*       NOPIXELS/ERROR/
*               CALLED IF THERE ARE NO VALID PIXELS
*
*CALLS
*       THIS PACKAGE:
*               GETPAR,GT2DIR,GTDSCR,IMGSUM,LBGONE.
*       STARLINK:
*               WRKEYR,WRUSER,WRERR,FRDATA.
*
*NOTES
*       USES VAX %VAL FACILITY
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      character cval*1,prbuf*50
      real av(1)
      integer npt(1)
 
*
* OBTAIN INTERACTION LEVEL
*
      ilevel=2
      call getpar('ILEVEL','INTEGER',1,1.0,3.0,.true.,ilevel,rval,ierr)
 
*
* OBTAIN INPUT IMAGE FRAME
*
      call gt2dir('INPUT',102,.false.,npix,nlines,ipin,ierrin)
 
      if(ierrin.eq.0) then
 
*
* INPUT IMAGE OBTAINED SUCCESSFULLY... EXTRACT INVALID FLAG, SCALE
* AND ZERO LEVEL FROM DESCRIPTOR
*
         inval=-100000
         scale=1.0
         zero=0.0
         call gtdscr('INPUT','INVAL','INTEGER',inval,rval,cval,ierr)
         call gtdscr('INPUT','BSCALE','REAL',ival,scale,cval,ierr)
         call gtdscr('INPUT','BZERO','REAL',ival,zero,cval,ierr)
 
*
* CALL IMGSUM TO SUM THE VALID IMAGE PIXELS
*
         call imgsum(%val(ipin),npix,nlines,inval,sum,npt(1))
 
*
* IF THE IMAGE HAS AT LEAST 1 VALID PIXEL, FIND THE AVERAGE
*
 
         if(npt(1).ge.1) then
            av(1)=(sum/npt(1))*scale+zero
 
*
* WRITE RESULT TO THE ENVIRONMENT
*
            call wrkeyr('AVERAGE',av,1,istat)
            call wrkeyi('NGOOD',npt,1,istat)
 
*
* IF ILEVEL IS 2 OR MORE, PRINT THE RESULT FOR THE USER
*
 
            if(ilevel.ge.2) then
               write(prbuf,10) npt(1),av(1)
10             format(' AVERAGE OF ',i8,' PIXEL(S) IS ',ss,g13.6)
 
*
* REMOVE UNWANTED BLANKS IN MESSAGE
*
               call lbgone(prbuf(13:))
               call wruser(' ',istat)
               call wruser(prbuf,istat)
               call wruser(' ',istat)
            endif
 
 
*
* IF THE IMAGE HAS NO VALID PIXELS... GIVE MESSAGE AND ABORT
*
 
         else
            call wrerr('NOPIXELS')
         endif
 
      endif
 
 
*
* RELEASE IMAGE FRAME AND RETURN
*
      call frdata(' ',istat)
      return
 
      end
 
 
 
