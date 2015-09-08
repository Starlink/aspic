      subroutine ikon_cls(n)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Clears overlay plane n where n is in the range 0 to 7
*
*METHOD
*       Set the Ikon fill colour to be one in the bits corresponding
*	to each overlay plane, except the plane to be cleared. Set
*       the fill mode to 'AND' and fill the entire overlay screen.
*       NB this is done with a 'draw filled rectangle' command because
*       the 'fill screen' command seems to ignore the fill mode
*       register. 
*       
*ARGUMENTS       
*   INPUTS:
*       n	integer		The plane to be cleared (0-7)
*
*SUBROUTINES CALLED
*	GKS:
*		gk8dwo
*
*VAX SPECIFICS
*       implicit none
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 27/6/88
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      integer n
*
* DECLARE LOCAL VARIABLES
*
      integer	code(9) ! Ikon Pixel Engine op. codes
      integer	icol	! Fill colour
      integer	nleft	! No. of bytes left in Ikon I/O buffer
      integer	plane	! Overlay plane in range 0 to 7
      integer	power(8)! Powers of two

      data power/1,2,4,8,16,32,64,128/
*
* CHECK PLANE IS IN CORRECT RANGE
*
      plane=mod(n,8)
*
* CALCULATE FILL COLOUR
*
      icol=255-power(plane+1)
*
* SET FILL COLOUR
*
      code(1)=256*69 + icol
*
* SET FILL MODE TO 'AND'. NB THE 'SET FILL MODE REGISTER' COMMAND
* DOESN'T SEEM TO WORK
*
      code(2)=256*96 + 42
      code(3)=          2
*
* MOVE TO BOTTOM  RIGHT CORNER
*
      code(4)=164
      code(5)=0
      code(6)=0
*
* DRAW A FILLED RECTANGLE COVERING ALL THE SCREEN
*
      code(7)=180
      code(8)=1023
      code(9)=779
*
* SEND THE CODE BUFFER TO THE IKON
*
      call gk8dwo(4,9,code,nleft)
      call gk8dwo(5,1,code,nleft)
*
* FINISH
*
      end
