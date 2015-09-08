      subroutine args_allwrt
*+
*   ARGS_ALLWRT
*
*   Sends ZWE1 FFFF and ZDI1 FFFF to ARGS so that all planes
*   are enabled.
*
*   Called:
*     args_flush,args_put1,srsend: ARGSLIB
*
*   J.A.Cooke 11Dec81
*-

*   clear space for the instruction in the buffer.....
      call args_flush(4)
      
*   opcode for ZWE1.....
      call args_put1('2D01'X)

*   parameter.....
      call args_put1('FFFF'X)

*   opcode for ZDI1.....
      call args_put1('2901'X)

*   parameter.....
      call args_put1('FFFF'X)

*   send the buffer.....
      call srsend

      end
