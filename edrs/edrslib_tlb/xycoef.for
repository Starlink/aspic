      subroutine xycoef
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO CALCULATE A SET OF 6 LINEAR TRANSFORMATION COEFFICIENTS
*       WHICH CORRESPOND TO A SPECIFIED SHIFT, ROTATION AND
*       MAGNIFICATION
*
*METHOD
*       OBTAIN INPUT PARAMETERS FOR TRANSFORMATION, CALCULATE
*       COEFFICIENTS AND WRITE TO THE ENVIRONMENT
*
*ARGUMENTS
*       NONE
*
*STARLINK PARAMETERS
*       ILEVEL
*               INTERACTION LEVEL: CONTROLS PRINTING OF RESULTS
*       CENTRE
*               CENTRE FOR ROTATION AND MAGNIFICATION
*       MAGNIFY
*               MAGNIFICATION FACTOR
*       ROTATION
*               ROTATION IN DEGREES (X THROUGH Y POSITIVE)
*       NEWCEN
*               NEW LOCATION OF CENTRE OF MAGNIFICATION AND ROTATION
*       TRCOEFFS
*               OUTPUT PARAMETER GIVING THE TRANSFORMATION COEFFICIENTS
*
*CALLS
*       THIS PACKAGE:
*               GETPAR,TRCOUT
*       STARLINK:
*               RDKEYR,WRUSER
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      character cval*1,prbuf*80
      real c(6),shift(2),centre(2)
 
*
* OBTAIN INTERACTION LEVEL
*
      ilevel=2
      call getpar('ILEVEL','INTEGER',1,1.0,3.0,.true.,inval,rval,ierr)
 
*
* OBTAIN CENTRE OF ROTATION AND MAGNIFICATION
*
      centre(1)=0.0
      centre(2)=0.0
      call rdkeyr('CENTRE',.true.,2,centre,nval,istat)
 
*
* OBTAIN MAGNIFICATION FACTOR
*
      rmag=1.0
      call getpar('MAGNIFY','REAL',1,-1.0e20,1.0e20,.true.,ival,rmag
     : ,ierr)
 
*
* OBTAIN ROTATION IN DEGREES, THEN CONVERT TO RADIANS
*
      rot=0.0
      call getpar('ROTATION','REAL',1,-1.0e20,+1.0e20,.true.,ival,rot
     : ,ierr)
      rot=rot*1.745329e-2
 
*
* OBTAIN NEW LOCATION OF CENTRE
*
      shift(1)=centre(1)
      shift(2)=centre(2)
      call rdkeyr('NEWCEN',.true.,2,shift,nval,istat)
 
*
* CALCULATE THE TRANSFORM COEFFICIENTS
*
      cs=cos(rot)
      sn=sin(rot)
      c(1)=shift(1)-centre(1)*rmag*cs+centre(2)*rmag*sn
      c(2)=rmag*cs
      c(3)=-rmag*sn
      c(4)=shift(2)-centre(1)*rmag*sn-centre(2)*rmag*cs
      c(5)=+rmag*sn
      c(6)=rmag*cs
 
*
* PRINT THE RESULTS
*
 
      if(ilevel.ge.2) then
         call wruser(' ',istat)
         call wruser('   TRANSFORMATION COEFFICIENTS:',istat)
         call wruser(' ',istat)
 
         do 44 j=1,4,3
            write(prbuf,19)((l,c(l)),l=j,j+2)
19          format(10x,3('C(',i1,')=',ss,g13.6,2x))
            call wruser(prbuf,istat)
44       continue
 
         call wruser(' ',istat)
      endif
 
 
*
* WRITE RESULTS TO THE ENVIRONMENT
*
      call trcout('TRCOEFFS',c,6,istat)
 
      end
 
 
 
