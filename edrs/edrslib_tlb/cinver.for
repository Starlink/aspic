      subroutine cinver
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO INVERT A LINEAR TRANSFORMATION BETWEEN IMAGE POSITIONS
*
*METHOD
*       OBTAIN PARAMETERS, CALCULATE INVERSE, PRINT RESULT AND WRITE
*       TO ENVIRONMENT
*
*ARGUMENTS
*       NONE
*
*STARLINK PARAMETERS
*       ILEVEL
*               INTERACTION LEVEL: CONTROLS PRINTING OF RESULTS
*       TRCOEFFS
*               INPUT TRANSFORMATION COEFFICIENTS
*       INVERSE
*               OUTPUT INVERSE TRANSFORMATION COEFFICIENTS
*       SINGULAR/ERROR/
*               CALLED IF INPUT TRANSFORMATION IS SINGULAR
*
*CALLS
*       THIS PACKAGE:
*               GETPAR,TRCOUT
*       STARLINK:
*               RDKEYR,WRUSER,WRERR
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      real c(6),d(6)
      character prbuf*80
 
*
* OBTAIN INTERACTION LEVEL
*
      ilevel=2
      call getpar('ILEVEL','INTEGER',1,1.0,3.0,.true.,ilevel,rval,ierr)
 
*
* SET DEFAULT TRANSFORMATION
*
      c(1)=0.0
      c(2)=1.0
      c(3)=0.0
      c(4)=0.0
      c(5)=0.0
      c(6)=1.0
 
*
* OBTAIN INPUT TRANSFORMATION
*
      call rdkeyr('TRCOEFFS',.false.,6,c,nval,istat)
 
*
* CALCULATE THE DETERMINANT
*
      det=c(2)*c(6)-c(3)*c(5)
 
*
* IF THE TRANSFORMATION IS NOT SINGULAR, CALCULATE THE INVERSE
*
 
      if(det.ne.0.0) then
         d(1)=(c(4)*c(3)-c(1)*c(6))/det
         d(2)=c(6)/det
         d(3)=-c(3)/det
         d(4)=(c(1)*c(5)-c(4)*c(2))/det
         d(5)=-c(5)/det
         d(6)=c(2)/det
 
*
* IF ILEVEL.GE.2, SHOW THE USER THE RESULT
*
 
         if(ilevel.ge.2) then
            call wruser(' ',istat)
            call wruser(' INVERSE TRANSFORMATION COEFFICIENTS:',istat)
            call wruser(' ',istat)
 
            do 44 j=1,4,3
               write(prbuf,43)((l,d(l)),l=j,j+2)
43             format(10x,3('C(',i1,')=',ss,g13.6,2x))
               call wruser(prbuf,istat)
44          continue
 
            call wruser(' ',istat)
         endif
 
 
*
* WRITE THE INVERSE TO THE ENVIRONMENT
*
         call trcout('INVERSE',d,6,istat)
 
      else
 
*
* IF TRANSFORMATION IS SINGULAR, GIVE MESSAGE
*
         call wrerr('SINGULAR')
      endif
 
      return
 
      end
 
 
 
