      subroutine args_usrcur(ovp,col,cur,xin,yin,xout,yout,butt)

*+  ARGS_USRCUR
*
*   Takes 64*64 bit user defined cursor, plotted from bottom left
*   ( +x +y ), plots it with bottom left pixel at position 
*   (xin,yin) (ARGS units), lights all buttons on the trackerball
*   unit .  After rolling the trackerball
*   and hitting any button, the routine returns the new position
*   in (xout,yout) of the bottom left hand pixel of the array.
*   All lights are extinguished, and a code is returned for the
*   button pressed.
*
*   Given:  (arguments)
*     OVP      I    overlay plane (8-15)
*     COL      C*1  overlay colour
*     CUR      I*2A cursor (256)
*     XIN      I    start position of cursor bottom lh corner
*     YIN      I      "       "     "    "      "    "    "
*
*   Returned:  (arguments)
*     XOUT     I    finishing position of cursor bottom lh corner
*     YOUT     I        "         "     "    "      "    "    "
*     BUTT     I    code for button pressed (1-4)
*
*   Called:
*     LOAD_BITCUR, WRITE_BITCUR, RUN_BITCUR, READ_BITCUR: local
*
*   J.A.Cooke/UOE/1Dec81
*-

      character col*(*),upcol*1
      integer xin,yin,xout,yout,ovp,butt
      integer*2 cur(256),idat(259),odat(3)

*   enable overlay in colour 'col'.....
      call str$upcase(upcol,col)
      call args_ovgen('W')
      call args_ovwrt(0)
      call args_ovcol(0,upcol)

*   input data to ARGS program.....
      do i=1,256
         idat(i)=cur(i)
      enddo
      idat(257)=ovp
      idat(258)=xin
      idat(259)=yin

*   load and run ARGS program.....
      call load_bitcur
      call write_bitcur(idat)
      call run_bitcur
      call read_bitcur(odat)

*   output data from ARGS program.....
      xout=odat(1)
      yout=odat(2)
      butt=odat(3)

      end
