      subroutine invert(c,d,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Inverts a linear transformation described by 6 co-efficients
*
*SOURCE
*       INVERT.FOR in UTILITIES.TLB
*
*ARGUMENTS
*   INPUTS:
*       c(6)    real    The input co-efficients
*   OUTPUTS:
*       d(6)    real    The co-efficients of the inverse of c
*       ierr    integer Error status: 0 - Success
*                                     1 - c is singular (not reported)
*
*USED BY
*       CONVOLVE
*
*SUBROUTINES CALLED
*       none
*
*VAX SPECIFICS
*       implicit none
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 21/11/87
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      real      c(6),d(6)
      integer   ierr
*
* DECLARE LOCAL VARIABLES
*
      real      det     ! The determinant of the transformation, c
*
* CALCULATE DETERMINANT OF C
*
      det=c(2)*c(6)-c(3)*c(5)
      if(det.ne.0) then
*
* CALCULATE INVERSE OF C
*
         d(1)=(c(4)*c(3)-c(1)*c(6))/det
         d(2)=c(6)/det
         d(3)=-c(3)/det
         d(4)=(c(1)*c(5)-c(4)*c(2))/det
         d(5)=-c(5)/det
         d(6)=c(2)/det
         ierr=0
*
* IF DETERMINANT IS ZERO, SET IERR=1
*
      else
         ierr=1
      endif
*
* FINISH
*
      end
