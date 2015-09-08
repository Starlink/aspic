      SUBROUTINE AUTRNG(IXEXT,IYEXT,ARRAY,TZEROD,TMAXD)
*+
*   AUTRNG
*
*   Finds the minimum and maximum pixel values in an image
*
*   Given         (arguments)
*   IXEXT    I     X-dimension of image array
*   IYEXT    I     Y-dimension of image array
*   ARRAY    RA    array containing image
*
*   Returned      (arguments)
*   TZEROD   R     minimum value in image
*   TMAXD    R     maximum value in image
*
*   B.D.Kelly/ROE/8.12.1981
*-

      INTEGER IXEXT,IYEXT
      REAL ARRAY(IXEXT,IYEXT),TZEROD,TMAXD

      TMAXD=ARRAY(1,1)
      TZEROD=TMAXD

      DO J=1,IYEXT
         DO I=1,IXEXT
            TMAXD=MAX(TMAXD,ARRAY(I,J))
            TZEROD=MIN(TZEROD,ARRAY(I,J))
         ENDDO
      ENDDO

      END
