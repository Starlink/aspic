      subroutine args_ovwrt(n)
*+
*   ARGS_OVWRT
*
*   Write enable ARGS pixel plane n (0-15)
*   Other planes are disabled for writing.
*
*   Given  (argument):
*	N	I	pixel plane (range 0-15)
*
*   Called:
*	args_flush,srgs_s1,srsend: ARGSLIB
*
*   J.A.Cooke/UOE/10Dec81
*-
      integer n,np,bit

*   mask n to range 0-15.....
      np=iand(n,'000F'X)

*   set a bit in the correct position for plane n.....
      bit=ishft('0001'X,np)

*   ZWE and ZDI this plane.....
      call args_flush(2)
      call args_s1('ZWE1',bit)
      call args_s1('ZDI1',bit)
      call srsend

      end
