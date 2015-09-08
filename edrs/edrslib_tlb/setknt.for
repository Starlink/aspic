      subroutine setknt(x,nx,xknot,nxknot)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO PLACE INTERIOR KNOTS SO AS TO MAINTAIN AN EVEN DISTRIBUTION
*       OF DATA POINTS BETWEEN THE KNOT POSITIONS WHEN FITTING SPLINE
*       FUNCTIONS TO DATA.
*
*METHOD
*       CALCULATE THE MEAN NUMBER OF DATA POINTS PER KNOT INTERVAL.
*       PLACE THE KNOTS THIS NUMBER OF DATA POINTS APART, INTERPOLATING
*       LINEARLY BETWEEN THE DATA POSITIONS TO OBTAIN THE KNOT POSITIONS
*
*ARGUMENTS
*       X (IN)
*       DOUBLE PRECISION(NX)
*               THE DATA POSITIONS
*       NX (IN)
*       INTEGER
*               THE NUMBER OF DATA POINTS
*       XKNOT (OUT)
*       DOUBLE PRECISION(NXKNOT)
*               THE INTERIOR KNOT POSITIONS
*       NXKNOT (IN)
*       INTEGER
*               THE REQUIRED NUMBER OF INTERIOR KNOTS
*
*CALLS
*       NONE
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      double precision x(nx),xknot(nxknot)
 
*
* FIND MEAN NUMBER OF DATA POINTS PER KNOT INTERVAL
*
      xintvl=(nx-1)/(nxknot+1.0)
 
*
* COUNT THROUGH EACH KNOT, CALCULATING THE NO. OF DATA POINTS
* LESS THAN THE REQUIRED KNOT VALUE
*
 
      do 1 i=1,nxknot
         datapt=1.0+xintvl*i
 
*
* THE RESULT IS NOT USUALLY AN INTEGER, SO INTERPOLATE LINEARLY
* BETWEEN THE ORDERED DATA POSITIONS
*
         ndat1=datapt
         ndat2=min(nx,ndat1+1)
         ddata=datapt-ndat1
         xknot(i)=x(ndat1)*(1.0-ddata)+x(ndat2)*ddata
1     continue
 
      return
 
      end
 
 
 
