      subroutine args_ovcol(n,col)
*+
*   ARGS_OVCOL
*
*   set colour of overlay plane n (8-15)
*
*   Given  (arguments):
*	N	I	overlay plane
*	COL	C*1	colour: W,R,G,B,Y,C,M; else black
*
*   Called:
*	str$upcase: vax Fortran library
*	args_decol: ROEARGS (from RGO)
*	args_rvt: ROEARGS
*	args_flush,args_put1,srsend: ARGSLIB
*
*   J.A.Cooke/UOE/10Dec81
*-
      integer n,vtt(4,256),addr,iv,icol,args_decol,idecol,np,bit
      character col*(*),upcol*1

*   get colour mask.....
      call str$upcase(upcol,col)
      idecol=args_decol(upcol)
      icol=ishft(idecol,12)

*   read vtt from ARGS.....
      call args_rvt(vtt,ist)

*   mask n to range 0-7 (for vtt address).....
      np=iand(n,'0007'X)

*   set address for correct plane.....
      bit=ishft('0001'X,np)
      addr=bit
      iv=addr+1

*   output ARGS LVT order.....
      call args_flush(4)
      call args_put1('1800'X)
      call args_put1(addr)
      call args_put1(vtt(2,iv)*256+vtt(1,iv))
      call args_put1(icol+vtt(3,iv))
      call srsend
 
      end
