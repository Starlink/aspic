      subroutine ncrcol(colour)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To set the current NCAR pen colour to the colour requested.
*
*SOURCE
*       NCRCOL.FOR in UTILITIES.TLB
*
*METHOD
*       Recall the user state variable for the requried colour from
*       the common block NCCOM and activate it. The user state variables
*       were set by routine NCROPN when NCAR was opened. If an unknown
*       colour is requested the colour is left as it is.
*
*ARGUMENTS
*   INPUTS:
*       colour  character       The requested colour. The known colours
*                               are defined in NC_COM, but depends also
*                               on the number of colours available on
*                               the current graphics device.
*
*SUBROUTINES CALLED
*       NCAR:
*               setusv
*
*VAX SPECIFICS
*       implicit none
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 22/3/88
*-------------------------------------------------------------------
*
      implicit none
*
* INCLUDE COMMON BLOCK NCCOM, DEFINING NCAR COLOUR INFORMATION
*
      include 'UTILITIES(NC_COM)'
*
* DECLARE ARGUMENTS
*
      character colour*(*)
*
* DECLARE LOCAL VARIABLES
*
      integer   icol    ! Colour loop count
*
* FIND THE REQUIRED COLOUR INDEX AND SET THE CURRENT INDEX TO IT
*
      do icol=1,NC_ncl
         if(colour.eq.NC_cnm(icol)) call setusv('II',NC_ind(icol))
      enddo
*
* FINISH
*
      end
