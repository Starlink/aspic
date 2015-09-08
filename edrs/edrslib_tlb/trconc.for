      subroutine trconc
 
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO CALCULATE A SET OF X,Y TRANSFORMATION COEFFICIENTS WHICH ARE
*       EQUIVALENT TO SUCCESSIVE APPLICATIONS OF TWO SEPARATE SETS OF
*       COEFFICIENTS.
*
*METHOD
*       THE TWO SETS OF COEFFICIENTS ARE COMBINED INTO ONE.
*
*ARGUMENTS
*       NONE
*CALLS
*       THIS PACKAGE:
*               GETPAR,TRCOUT
*       STARLINK:
*               RDKEYR,WRUSER
*
*WRITTEN BY:
*       D.S. BERRY, TIDIED UP BY R.F. WARREN-SMITH
*----------------------------------------------------------------------
*
*
      character pr*80
      real a(6),b(6),c(6)
 
*
* SET DEFAULTS FOR INPUT COEFFICIENTS
*
      data a/0.0,1.0,0.0,0.0,0.0,1.0/
      data b/0.0,1.0,0.0,0.0,0.0,1.0/
 
*
* OBTAIN INTERACTION LEVEL
*
      ilevel=2
      call getpar('ILEVEL','INTEGER',1,1.0,3.0,.true.,ilevel,rval,ierr)
 
*
* OBTAIN INPUT SETS OF COEFFICIENTS
*
      call rdkeyr('TRA',.false.,6,a,nval,istat)
      call rdkeyr('TRB',.false.,6,b,nval,istat)
 
*
* COMBINE THE TWO TRANSFORMATIONS
*
      c(1)=b(1)+b(2)*a(1)+b(3)*a(4)
      c(2)=b(2)*a(2)+b(3)*a(5)
      c(3)=b(2)*a(3)+b(3)*a(6)
      c(4)=b(4)+b(5)*a(1)+b(6)*a(4)
      c(5)=b(5)*a(2)+b(6)*a(5)
      c(6)=b(5)*a(3)+b(6)*a(6)
 
*
* DISPLAY THE RESULT
*
 
      if(ilevel.ge.2)then
         call wruser(' ',istat)
         write(pr,18)
18       format(3x,'COMBINED TRANSFORMATION COEFFICIENTS:')
         call wruser(pr(1:lens(pr)),istat)
         call wruser(' ',istat)
 
         do 44 j=1,4,3
            write(pr,19)((l,c(l)),l=j,j+2)
19          format(10x,3('C(',i1,')=',ss,g13.6,2x))
            call wruser(pr(1:lens(pr)),istat)
44       continue
 
         call wruser(' ',istat)
      endif
 
 
*
* WRITE THE RESULT TO THE ENVIRONMENT
*
      call trcout('TRCOEFFS',c,6,istat)
 
      end
 
 
 
