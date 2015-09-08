      subroutine drline(x,y,holxlo,holxhi,holylo,holyhi,first)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Appends a new point to an SGS poly line, ensuring that a given
*       range in x has nothing drawn in it.
*
*SOURCE
*       DRLINE.FOR in UTILITIES.TLB
*
*METHOD
*       Determine if the new point is below, inside or above the
*       forbidden range (or hole). For each of these possibilities, go
*       through the possibilities for the last point (saved form last
*       time through the routine) and determine if any lines should be
*       drawn. The line may be drawn in either direction, so the hole
*       may be approached from below or above.
*
*ARGUMENTS
*   INPUTS:
*       y       real    The y co-ord of the new point
*       x       real    The x co-ord of the new point
*       holxhi  real    The upper limit of the forbidden x range
*       holxlo  real    The lower limit of the forbidden x range
*       first   logical True if this is the first point in the poly line
*   OUTPUTS:
*       holyhi  real    The y value of the point at which the polyline
*                       leaves the hole (zero if hole has not yet been
*                       left)
*       holylo  real    The y value of the point at which the polyline
*                       enters the hole (zero if hole has not yet been
*                       entered)
*
*USED BY
*       CRDDTRACE
*
*SUBROUTINES CALLED
*       SGS:
*               sgs_apoly,sgs_opoly,sgs_bpoly
*
*VAX SPECIFICS
*       implicit none
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 7/9/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      real      y,x,holxlo,holxhi,holylo,holyhi
      logical   first
*
* DECLARE LOCAL VARIABLES
*
      logical   above   ! True if the new point is above the hole
      logical   below   ! True if the new point is below the hole
      logical   inside  ! True if the new point is inside the hole
      logical   lastab  ! True if the last point was above the hole
      logical   lastbe  ! True if the last point was below the hole
      logical   lastin  ! True if the last point was inside the hole
      real      xlast   ! Last times x value
      real      yhi     ! Buffer for holyhi
      logical   yhifnd  ! True if the lower hole limit has been found
      real      ylast   ! Last times y value
      real      ylo     ! Buffer for holylo
      logical   ylofnd  ! True if the upper hole limit has been found
*
* SAVE LAST TIMES VALUES
*
      save lastbe,lastab,lastin,xlast,ylast,ylo,yhi
*
* SEE IF NEW POINT IS BELOW, INSIDE OR ABOVE THE HOLE
*
      above=.false.
      inside=.false.
      below=.false.
      if(x.gt.holxhi) then
         above=.true.
      else if(x.lt.holxlo) then
         below=.true.
      else
         inside=.true.
      endif
*
* IF THIS IS THE FIRST POINT IN THE POLY LINE START A POLYLINE SO LONG
* AS THE POINT IS NOT INSIDE THE HOLE, AND FLAG THAT THE HOLE HAS NOT
* BEEN FOUND
*
      if(first) then
         first=.false.
         if(.not.inside) then
            call sgs_bpoly(x,y)
         endif
         ylofnd=.false.
         yhifnd=.false.
         ylo=y
         yhi=y
      else
*
* IF THE NEW POINT IS BELOW THE HOLE, ...
*
         if(below) then
*
* AND THE LASTPOINT WAS ALSO BELOW THE HOLE, JUST APPEND THE NEW POINT
* TO THE CURRENT POLYLINE
*
            if(lastbe) then
               call sgs_apoly(x,y)
*
* OTHERWISE, IF THE LAST POINT WAS INSIDE THE HOLE, START A NEW
* POLYLINE BY DRAWING THE VISIBLE (LOWER) END OF THE CURRENT LINE
*
            else if(lastin) then
               ylo=ylast+(holxlo-xlast)*(y-ylast)/(x-xlast)
               ylofnd=.true.
               call sgs_bpoly(holxlo,ylo)
               call sgs_apoly(x,y)
*
* OTHERWISE, IF THE LAST POINT WAS ABOVE THE HOLE DRAW THE CURRENT LINE
* WITH A HOLE IN IT
*
            else
               yhi=ylast+(holxhi-xlast)*(y-ylast)/(x-xlast)
               yhifnd=.true.
               call sgs_apoly(holxhi,yhi)
               call sgs_opoly
               ylo=ylast+(holxlo-xlast)*(y-ylast)/(x-xlast)
               ylofnd=.true.
               call sgs_bpoly(holxlo,ylo)
               call sgs_apoly(x,y)
            endif
*
* IF THE NEW POINT IS INSIDE THE HOLE....
*
         else if(inside) then
*
* AND IF THE LAST POINT WAS BELOW THE HOLE THEN DRAW THE PART OF THE
* CURRENT LINE WHICH IS BELOW THE HOLE TO FINISH THE CURRENT POLYLINE
*
            if(lastbe) then
               ylo=ylast+(holxlo-xlast)*(y-ylast)/(x-xlast)
               ylofnd=.true.
               call sgs_apoly(holxlo,ylo)
               call sgs_opoly
*
* OTHERWISE IF THE LAST POINT WAS ABOVE THE HOLE, DRAW THE UPPER
* END OF THE  CURRENT LINE
*
            else if(lastab) then
               yhi=ylast+(holxhi-xlast)*(y-ylast)/(x-xlast)
               yhifnd=.true.
               call sgs_apoly(holxhi,yhi)
               call sgs_opoly
            endif
*
* IF THE NEW POINT IS ABOVE THE HOLE...
*
         else
*
* AND THE LAST POINT WAS BELOW IT, DRAW BOTH THE PART OF THE LINE
* VISIBLE BELOW THE HOLE AND THE PART VISIBLE ABOVE THE HOLE
*
            if(lastbe) then
               ylo=ylast+(holxlo-xlast)*(y-ylast)/(x-xlast)
               ylofnd=.true.
               call sgs_apoly(holxlo,ylo)
               call sgs_opoly
               yhi=ylast+(holxhi-xlast)*(y-ylast)/(x-xlast)
               yhifnd=.true.
               call sgs_bpoly(holxhi,yhi)
               call sgs_apoly(x,y)
*
* OTHERWISE IF THE LAST POINT WAS INSIDE THE HOLE, DRAW THE PART
* VISIBLE ABOVE THE HOLE
*
            else if(lastin) then
               yhi=ylast+(holxhi-xlast)*(y-ylast)/(x-xlast)
               yhifnd=.true.
               call sgs_bpoly(holxhi,yhi)
               call sgs_apoly(x,y)
*
* OTHERWISE IF THE LAST POINT WAS ALREADY ABOVE THE HOLE, JUST ADD THE
* NEW POINT TO THE POLYLINE.
*
            else
               call sgs_apoly(x,y)
            endif
         endif
      endif
*
* IF THE TRACE IS MOVING FROM THE UPPER END TO THE LOWER END (THE HOLE
* IS ASSUMED TO BE AT THE LOWER END) AND THE HOLE HAS NOT BEEN FOUND,
* SET THE Y LIMITS OF THE HOLE TO THE CURRENT Y VALUE
*
      if(x.lt.xlast) then
         if(.not.yhifnd) yhi=y
         if(.not.ylofnd) ylo=y
      endif
*
* SAVE LAST TIMES VALUES
*
      lastbe=below
      lastab=above
      lastin=inside
      xlast=x
      ylast=y
*
* RETURN THE UPPER AND LOWER LIMITS OF Y AT THE HOLE
*
      holylo=ylo
      holyhi=yhi
*
* FINISH
*
      end
