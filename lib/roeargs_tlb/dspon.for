      subroutine args_dspon(n,switch)
*+
*   ARGS_DSPON
*
*   Enable ARGS pixel plane n (0-15) to display.
*   No change to other planes.
*
*   Given  (arguments):
*	N	I	plane 0-15
*	SWITCH	I	bit pattern of enabled planes
*			from args_dspall
*
*   Returned  (arguments):
*	SWITCH	I	altered bit pattern
*
*   Called:
*	args_flush,args_s1,srsend:  ARGSLIB
*
*   J.A.Cooke/UOE/17Dec81
*-
      integer n,switch,mask,np

*   mask n to range 0-15.....
      np=iand(n,'000F'X)

*   set a bit in correct position for plane n.....
      mask=ishft('0001'X,np)

*   set switch.....
      switch=ior(switch,mask)

*   send new switch to ARGS.....
      call args_flush(2)
      call args_s1('ZDO1',switch)
      call srsend

      end
