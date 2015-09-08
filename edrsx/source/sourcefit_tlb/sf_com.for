*       module  SF_COM
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*
*SOURCE
*       SF_COM.INC in SOURCEFIT.TLB
*
*VAX SPECIFICS
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 12/9/91
*-------------------------------------------------------------------
*
* DECLARE VARIABLES
*
      real*8	SF_a
      real*8	SF_b
      real*8	SF_c( maxvar )
      real*8    SF_k( maxvar )
      integer   SF_inv
      integer	SF_ipi
      integer   SF_nln
      integer	SF_npx
      integer   SF_xlo
      integer   SF_xhi
      integer   SF_ylo
      integer   SF_yhi

*
* DECLARE COMMON BLOCK.
*
      common /SF_COM/ SF_inv, SF_ipi, SF_nln, SF_npx, SF_xlo, 
     :                SF_xhi, SF_ylo, SF_yhi, SF_a, SF_b, SF_c,
     :                SF_k

