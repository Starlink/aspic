      subroutine objfun( mode, m, n, ldfj, x, f, fjac, nstate, iuser, 
     :                   user )
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	
*	
*
*SOURCE
*       OBJFUN.FOR in SOURCEFIT.TLB
*
*METHOD
*       
*ARGUMENTS       
*   INPUT:
*
*   OUTPUTS:
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              
*       THIS PACKAGE (MEMCRDD.TLB):
*              
*       EDRS:
*              
*       INTERIM:
*              
*STARLINK PARAMETERS
*
*
*VAX SPECIFICS
*       implicit none
*       %val
*       do while
*       REAL*8
*       Trig functions in degrees
*       enddo
*       end of line comments
*       2 byte integer values
*       RTL routines
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 12/9/91
*-------------------------------------------------------------------
      implicit none

*
* DECLARE ARGUMENTS
*
      integer mode, m, n, ldfj, nstate, iuser(*)
      real*8    x(n), f(m), fjac(ldfj,n), user(*)

* 
* INCLUDE LOCAL PARAMETER VALUES.
*
      include '(SF_PAR)'

* 
* INCLUDE COMMON VALUES.
*
      include '(SF_COM)'

*
* CALL A LOWER LEVEL ROUTINE TO DO THE WORK.
*
      call objfn1( SF_npx, SF_nln, %val( SF_ipi ), mode, m, n, ldfj, x, 
     :             f, fjac )

*
* FINISH
*
      end
