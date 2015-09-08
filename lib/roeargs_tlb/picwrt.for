      subroutine args_picwrt
*+
*   ARGS_PICWRT
*
*   Sends ZWE1 00FF to ARGS so that pictures are written into planes
*   0 to 7 only;  sends ZDI1 00FF so graphics are written into
*   same planes.
*
*   Called:
*     args_flush,args_put1,srsend: ARGSLIB
*
*   J.A.Cooke 23Nov81:11Dec81:19Feb82
*-

*   clear space for the instruction in the buffer.....
      call args_flush(4)
      
*   ZWE1.....
      call args_put1('2D01'X)
      call args_put1('00FF'X)

*   ZDI1.....
      call args_put1('2901'X)
      call args_put1('00FF'X)

*   send the buffer.....
      call srsend

      end
