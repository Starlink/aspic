      subroutine args_reccur(ovp,col,xin,yin,xsiz,ysiz,xout,yout,ibutt)

*+  ARGS_RECCUR
*
*   Display a rectangular 'cursor' on the ARGS, allow the user to
*   move it around using the trackerball and return its position
*   when he presses any button.
*
*   Modified J.A.Cooke/UOE/20Nov81;14Dec81
*     Returns button code (1 to 4) in 'ibutt'
*
*   Given:
*     OVP      I     overlay plane (8-15)
*     COL      C     colour of cursor (only first char is significant -
*                    any primary or secondary or white or black)
*     XIN      I     starting x posn of cursor bottom lh corner (ARGS units)
*     YIN      I     starting y posn of cursor bottom lh corner (ARGS units)
*     XSIZ     I     size of cursor in X dirn (ARGS units)
*     YSIZ     I     size of cursor in Y dirn (ARGS units)
*
*     (XIN,YIN) is the position of the actual bottom lf corner of the
*     cursor (not of the bottom lh corner of the area enclosed by the
*     cursor). (XIN+XSIZ,YIN+YSIZ) is the position of the top rh corner
*     of the cursor (with the same caveat).
*
*   Returned:
*     XOUT     I     finishing x posn of cursor bottom lh corner (ARGS units)
*     YOUT     I     finishing y posn of cursor bottom lh corner (ARGS units)
*     IBUTT    I     code for button pressed (1 to 4)
*
*   Called:
*     args_ovgen,args_ovwrt,args_ovcol: ROEARGS
*     LOAD_RCUR, WRITE_RCUR, RUN_RCUR, READ_RCUR: local
*     STR$UPCASE: VMS
*
*   W F Lupton RGO (at ROE) 17th November 1981
*-
      integer xin,yin,xsiz,ysiz,xout,yout,ibutt,ovp,np,zdi
      integer*2 idat(5),odat(3)
      character col*(*),upcol*1

*   enable overlay in colour 'col'
      call str$upcase (upcol,col)
      call args_ovgen('W')
      call args_ovwrt(ovp)
      call args_ovcol(ovp,upcol)

*   calculate ZDI value.....
      np=ior('0008'X,iand(ovp,'000F'X))
      zdi=ishft('0001'X,np)

*   input data to ARGS program 
      if (zdi.eq.'8000'X) then
         idat(1)='8000'X
      else
         idat(1)=zdi
      endif
      idat(2) = xin
      idat(3) = yin
      idat(4) = xsiz
      idat(5) = ysiz

*   load and run ARGS program
      call load_rcur
      call write_rcur (idat)
      call run_rcur
      call read_rcur (odat)

*   output data from ARGS program
      xout = odat(1)
      yout = odat(2)
      ibutt = odat(3)

      end
