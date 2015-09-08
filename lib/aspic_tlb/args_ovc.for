      subroutine args_ovc (n,col)

*+  Set/reset overlay bits in entry 'n' of lookup table, to give overlay of
*   colour 'col'. Only the first character of 'col' is significant
*   (see 'args_decol') and the appropriate bits must have been enabled
*   globally. The current lookup table is read from the ARGS and the
*   appropriate overlay bits set in the above specified entry. 

C /CARGS1/ BUFFER HOLDING ORDERS FOR TRANSMISSION TO ARGS
C   IBUFF  BUFFER
C   L16    NUMBER OF 16-BIT WORDS IN IBUFF (SET IN BLOCK DATA)
C   SZHEAD NUMBER OF 16-BIT WORDS IN HEADER OF BUFFER
C   IUSED  NUMBER OF 16-BIT WORDS ACTUALLY USED
C   LPRT   TRUTH VALUE OF STMT "BUFFER TO BE PRINTED"
C   LARGSD TRUTH VALUE OF STMT "BUFFER TO BE OUTPUT TO ARGS VIA DMA"
C   LARGSS TRUTH VALUE OF STMT "BUFFER TO BE OUTPUT TO ARGS VIA
C          SERIAL INTERFACE"
C   CHAN   CHANNEL NUMBER FOR ARGS PARALLEL INTERFACE
C NON F66 -- USE OF INTEGER*2
      INTEGER*2 IBUFF
      INTEGER SZHEAD,CHAN
      LOGICAL LPRT,LARGSD,LARGSS
      COMMON /CARGS1/ IBUFF(520),L16,SZHEAD,IUSED,LPRT,LARGSD,LARGSS,
     1  CHAN
      integer n,args_decol
	byte vlut(4,256)
      integer*2 ioff(2),ion(2),iflip(2)
	integer*2 maskr,maskg,maskb
	integer*2 rdins(5)
	byte buff(10)
      character col
      data ioff,ion,iflip/6*0/

	call args_flush(-1)
	rdins(5)='5502'x
	call args_idcallrd(1,chan,rdins,5,buff,5,nok)
	if (buff(9).lt.55.and.buff(10).eq.67) then
		maskn = iand (n,'00FF'X)
		ion(1) = ior (maskn,ishft (maskn,8))
		ion(2) = ior (maskn,ishft (args_decol (col),12))
		call args_lvt (maskn,ioff,ion,iflip)

	else

	call asp_vltget(vlut)
	maskg=vlut(2,n+1)
	if (maskg.lt.0) maskg=maskg+256
	maskr=vlut(1,n+1)
	if (maskr.lt.0) maskr=maskr+256
	maskb=vlut(3,n+1)
	if (maskb.lt.0) maskb=maskb+256
	maskr=iand(maskr,'00FF'X)
	maskg=iand(maskg,'00FF'X)
	maskb=iand(maskb,'00FF'X)
      ion(1) = ior (maskr,ishft (maskg,8))
      ion(2) = ior (maskb,ishft (args_decol (col),12))

      call args_lvt (n,ioff,ion,iflip)

	endif
      end
