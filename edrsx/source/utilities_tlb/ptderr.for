      subroutine ptderr(descr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To give an error message saying that the descriptor contained
*       in argument descr could not be accessed properly.
*
*SOURCE
*       PTDERR.FOR in UTILITIES.TLB
*
*ARGUMENTS
*   INPUTS:
*       descr   character       The name of the descriptor which
*                               could not be accessed.
*
*USED BY
*       CRDDTRACE
*
*SUBROUTINES CALLED
*       EDRS:
*               lbgone
*       INTERIM:
*               wruser
*
*VAX SPECIFICS
*       implicit none
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 4/9/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      character descr*(*)
*
* DECLARE LOCAL VARIABLES
*
      integer   ierr    ! Status from wruser
*
* REMOVE LEADING BLANKS FROM THE DESCRIPTOR NAME
*
      call lbgone(descr)
*
* OUTPUT THE MESSAGE
*
      call wruser(' *** Error reading descriptor '//descr,ierr)

      end
