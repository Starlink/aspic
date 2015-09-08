      subroutine args_dspall(switch)
*+
*   ARGS_DSPALL
*
*   Enables all 16 planes of ARGS to display.
*   Sets 16 bits in 'switch'.
*
*   Returned  (argument):
*	SWITCH	I	bit pattern of enabled planes
*			Do not alter this from FORTRAN
*
*   Called:
*     args_flush,args_s1,srsend:  ARGSLIB
*
*   J.A.Cooke/UOE/17Dec81
*-
      integer switch

*   clear space for instruction.....
      call args_flush(2)

*   send ZDO1.....
      call args_s1('ZDO1','FFFF'X)

      call srsend

*   set switch.....
      switch='FFFF'X

      end
