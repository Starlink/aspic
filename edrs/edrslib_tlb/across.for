      subroutine across(id,x,y,lx,ly)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO PLOT A CROSS ON THE ARGS OF A SPECIFIED SIZE AT GIVEN USER
*       COORDINATES
*
*METHOD
*       USES THE ARGS DATABASE TO DETERMINE THE USER TO PIXEL
*       TRANSFORMATION, THEN PLOTS USING ARGS_POLYL
*
*ARGUMENTS
*       ID  (IN)
*       INTEGER
*              THE ID OF THE ARGS IMAGE IN WHOSE COORDINATE FRAME THE
*              CROSS IS PLOTTED
*       X,Y (IN)
*       REAL
*              THE X,Y COORDINATES OF THE CROSS CENTRE IN USER UNITS
*       LX,LY (IN)
*       INTEGER
*              THE SIZE OF THE CROSS IN ARGS PIXELS IN THE X AND Y
*              DIRECTIONS
*
*CALLS
*       ARGS_:
*               UTOP,PTOU,POLYL
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      real xx(2),yy(2)
 
*
* FIND THE X AND Y SHIFT IN USER COORDINATES CORRESPONDING TO
* THE ARMS OF THE CROSS
*
      call args_utop(id,x,y,ixcen,iycen,istat)
      call args_ptou(id,ixcen+lx,iycen+ly,ux,uy,istat)
 
*
* CALCULATE THE USER COORDINATES OF THE ENDS OF THE ARMS
*
      dx=ux-x
      dy=uy-y
      xx(1)=x-dx
      xx(2)=x+dx
      yy(1)=y
      yy(2)=y
 
*
* PLOT ONE PAIR OF ARMS
*
      call args_polyl(id,2,xx,yy,istat)
      xx(1)=x
      xx(2)=x
      yy(1)=y+dy
      yy(2)=y-dy
 
*
* PLOT THE OTHER PAIR
*
      call args_polyl(id,2,xx,yy,istat)
 
      end
 
 
 
