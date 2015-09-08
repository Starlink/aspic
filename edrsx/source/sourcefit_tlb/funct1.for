      subroutine funct1( n, xc, fc )
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	
*	
*
*SOURCE
*       FUNCT1.FOR in SOURCEFIT.TLB
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
* INCLUDE LOCAL PARAMETER DECLARATIONS
*
      include '(SF_PAR)'

*
* INCLUDE COMMON BLOCK.
*
      include '(SF_COM)'

*
* DECLARE ARGUMENTS
*
      integer	n
      real      xc(n),fc

*
* DECLARE LOCAL VARIABLES
*
      integer	i
      integer	j

*
* STORE THE TRIAL PARAMETER VALUES.
*
      if( SF_typ(:5) .eq. 'POWER' ) then
         do i = 1, SF_npk
            SF_pwr(i) = xc(i)
         end do

      else if( SF_typ .eq. 'RADIUS' ) then
         do i = 1, SF_npk
            SF_rad(i) = xc(i)
         end do

      else if( SF_typ(:4) .eq. 'PEAK' ) then
         do i = 1, SF_npk
            SF_pk(i) = xc(i)
         end do

      else if( SF_typ(:2) .eq. 'XY' ) then
         do i = 1, SF_npk
            j = 2*i - 1
            SF_xc(i) = xc(j)
            SF_yc(i) = xc(j+1)
         end do

      else if( SF_typ(:4) .eq. 'BACK' ) then
         do i = 1, SF_npk
            SF_bck(i) = xc(i)
         end do

      end if

*
* CALL FSUB TO CALCULATE THE RMS BETWEEN THE INPUT IMAGE AND THE 
* CURRENT MODEL.
*
      call fsub( %val( SF_ipi ), SF_npx, SF_nln, fc )

*
* NORMALIZE THE RMS RESIDUAL BY THE RMS RESIDUAL USING THE INITIAL
* GUESSES AT THE PARAMETER VALUES.
*
      fc = fc/SF_nm

*
* FINISH
*
      end
