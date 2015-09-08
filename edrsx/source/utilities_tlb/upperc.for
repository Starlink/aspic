      subroutine upperc(string)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Converts a string to upper case.
*
*SOURCE
*       UPPERC.FOR in UTILITIES.TLB
*
*METHOD
*       Call VAX run-time-library routine.
*
*ARGUMENTS
*   INPUTS:
*       string  character       String to be converted to upper case
*   OUTPUTS:
*       string  character       String converted to upper case
*
*SUBROUTINES CALLED
*       Run-Time-Library:
*               str$upcase
*
*VAX SPECIFICS
*       implicit none
*       end of line comments
*       tabs
*       RTL routines
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 15/3/88
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      character string*(*)
*
* CALL RTL ROUTINE TO DO CONVERSION
*
      call str$upcase(string,string)
*
* FINISH
*
      end
