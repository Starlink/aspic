      SUBROUTINE PUTHEX(IXIN,JYIN,ILEN,JLEN,PICTURE)
*-
*   PUTHEX
*
*   Sets individual bits within a 2-D 'bit array'.
*
*   Given         (arguments)
*   IXIN     I     X-coordinate of bit to be set
*   JYIN     I     Y-coordinate of bit to be set
*   ILEN     I     X-dimension of containing I*2 array
*   JLEN     I     Y-dimension of containing I*2 array
*   PICTURE  I*2A  containing array
*
*   Returned      (arguments)
*   PICTURE  I*2   modified containing array
*
*   B.D.Kelly/ROE/3.12.1981
*-

      INTEGER IXIN,JYIN,ILEN,JLEN
      INTEGER*2 PICTURE(ILEN,JLEN)
      INTEGER*2 HEX(16)

      DATA HEX/'1'X,'2'X,'4'X,'8'X,'10'X,'20'X,'40'X,'80'X,
     :         '100'X,'200'X,'400'X,'800'X,'1000'X,'2000'X,
     :         '4000'X,'8000'X/

      IEL=(IXIN-1)/16+1
      IBIT=1+MOD((IXIN-1),16)

      PICTURE(IEL,JYIN)=IOR(PICTURE(IEL,JYIN),HEX(IBIT))

      END
